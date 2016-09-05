{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- | Add name-based virtual hosting onto yesod server.
-- Works like this:
-- * The original yesod server works as the "default" host,
--   and contains all the real logic.
--   In this default host, routes that some path pieces under a certain common dir
--   is for a virtual host.
--
-- * A wai middleware that convert request to make it like a request
--   of default request but with path remapped, with the virtual host and its domain name
--   stored in vault of current request.
--
module Yesod.Helpers.VirtualHost where

import           ClassyPrelude.Yesod hiding (Request, Builder, Proxy)
import           Control.Monad.Trans.Maybe
import           Blaze.ByteString.Builder   (Builder)
import           Data.Proxy                 (Proxy(..))
import qualified Data.Vault.Lazy            as V
import           Network.URI                (parseURI, uriRegName, uriAuthority, uriToString)
import           Network.Wai                (Middleware, pathInfo, requestHeaderHost, vault, Request)


-- | Setup some rules about pathes so that we can construct virtual host
-- by looking at the pathes.
class VirtualHostPath a where
  -- | The path pieces prefix that this virtual host is placed in "default" host
  virtualHostPathPrefix :: a -> [Text]

  -- | The reverse of virtualHostPathPrefix.
  -- Given the path pieces of the original request,
  -- we need to find out what virtual host that the request should be mapped to.
  -- If possible, return the virtual host and remaining path pieces
  virtualHostFromPath :: [Text] -> Maybe (a, [Text])

  -- | if an original request path should be kept unchanged (don't add virtualHostPathPrefix)
  -- Usually these request pathes are considered to be as "shared" with default host
  virtualHostIfPathNotPrefixed :: Proxy a -> [Text] -> Bool


-- | How domain name to be mapped to virtual host
class VirtualHostDomain a where
  -- | To construct an 'a', we may need some extra info, besides the domain name
  type VirtualHostDomainExtra a :: *

  -- | to retrieve virtual host info by looking up domain name
  virtualHostByDomainName :: VirtualHostDomainExtra a -> Text -> IO (Maybe a)

  -- | Quickly determinate if a domain name is not mapped to any virtual host
  virtualHostExcludeDomainName :: Proxy a -> Text -> Bool
  virtualHostExcludeDomainName _ _ = False


-- | A simple cache to save the mapping from domain name to virtual host
-- Text part is the domain name.
-- 'a' part is the virtual host.
-- This cache is useful, because normally domain-to-host mapping is very stable.
type VirtualHostNameCache a = IORef (Map Text a)


-- | Put information about virtual host of current request
-- into vault of it. This is the key.
-- The vaule in stored in vault is the virtual host and its domain name.
type VirtualHostVaultKey s = V.Key (s, Text)

class HasVirtualHostVaultKey s a where
  getVirtualHostKey :: a -> VirtualHostVaultKey s

instance HasVirtualHostVaultKey s (VirtualHostVaultKey s) where
  getVirtualHostKey = id


-- | Get approot of the "default" host
class HasMasterApproot a where
  getMasterApproot :: a -> Text


-- | A middleware to convert the original request to request in default host, if possible.
-- It works by following these rules:
-- 1. Some domains are considered to be of default host, if it is the case, do nothing.
-- 2. You need to provide a function to map domain names to virtual hosts.
--    If domain name cannot be mapped to any virtual host, change nothing.
-- 3. If the path of the original request is considered to be shared with default host,
--    do nothing.
-- 4. If not forbidden by other rules, add path prefix to the original request path,
--    and save virtual host data into vault of the request.
--
-- CAUTION: The problem of this logic is you need to manually coordinate the settings of routes
--          and the functions params (those functions to test paths).
nameBasedVirtualHostMiddleware :: forall s. (VirtualHostPath s, VirtualHostDomain s)
                               => VirtualHostVaultKey s
                               -> VirtualHostDomainExtra s
                               -> VirtualHostNameCache s
                               -> Middleware
nameBasedVirtualHostMiddleware k vh_extra cache app req respond_func = do
  m_req2 <- runMaybeT $ do
    -- guard $ not $ any (flip isPrefixOf (pathInfo req)) shared_path
    guard $ not $ virtualHostIfPathNotPrefixed proxy_s (pathInfo req)
    virtual_domain <- MaybeT $ return m_virtual_domain
    m_svs <- lookup virtual_domain <$> readIORef cache

    svs <- case m_svs of
              Nothing -> do
                svs <- MaybeT $ virtualHostByDomainName vh_extra virtual_domain

                atomicModifyIORef' cache ((, ()) . insertMap virtual_domain svs)
                return svs

              Just svs -> return svs

    let path_prefix = virtualHostPathPrefix svs

    let new_path_info = path_prefix <> pathInfo req
    -- error $ show new_path_info

    return $ req { pathInfo = new_path_info
                 , vault = V.insert k (svs, virtual_domain) (vault req)
                 }

  app (fromMaybe req m_req2) respond_func

  where
    proxy_s = Proxy :: Proxy s

    m_virtual_domain = do
      host_name <- fmap decodeUtf8 $ requestHeaderHost req
      let host_domain = toLower $ takeWhile (/= ':') host_name

      -- 只要不是主域名，就认为是虚拟服务器的域名
      guard $ not $ virtualHostExcludeDomainName proxy_s host_domain

      return host_domain


-- | To be used when implementing 'approot' method of Yesod class.
-- Like this: approot = ApprootRequest (virtualHostAppRoot (Proxy :: Proxy XXX))
-- With this function, routes of virtual host will be rendered correct domain name.
virtualHostAppRoot :: forall s master. (HasVirtualHostVaultKey s master, HasMasterApproot master)
                   => Proxy s
                   -> master
                   -> Request
                   -> Text
virtualHostAppRoot _ foundation req =
  fromMaybe master_approot $ do
    (_ :: s, domain) <- V.lookup k (vault req)
    uri <- parseURI (unpack master_approot)
    uri_auth <- uriAuthority uri
    return $ fromString $ flip (uriToString id) "" $ uri { uriAuthority = Just (uri_auth { uriRegName = unpack domain }) }
  where
    k              = getVirtualHostKey foundation
    master_approot = getMasterApproot foundation


-- | To be used when implementing 'joinPath' method of Yesod class.
-- Like this: joinPath = virtualHostJoinPath (Proxy :: Proxy XXX)
virtualHostJoinPath :: forall s master. (VirtualHostPath s, HasMasterApproot master)
                    => Proxy s
                    -> master
                    -> Text
                    -> [Text]
                    -> [(Text, Text)]
                    -> Builder
virtualHostJoinPath _ foundation app_root path_pieces params =
  joinPath dummy_yesod app_root new_path_pieces params
  where
    new_path_pieces = if app_root == master_approot
                        then path_pieces
                        else path_pieces2
    path_pieces2 = fromMaybe path_pieces $ do
      (_ :: s, ps) <- virtualHostFromPath path_pieces
      return ps

    dummy_yesod = LiteApp $ \_ _ -> Nothing
    master_approot = getMasterApproot foundation
