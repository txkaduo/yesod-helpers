{-# LANGUAGE ScopedTypeVariables #-}
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

import           ClassyPrelude hiding (Builder)
import           Yesod
import           Control.Monad.Trans.Maybe
import           Blaze.ByteString.Builder   (Builder, toByteString)
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
  virtualHostPathShouldNotPrefix :: Proxy a -> [Text] -> Bool


-- | How domain name to be mapped to virtual host
class VirtualHostFromDomain a where
  -- | To construct an 'a', we may need some extra info, besides the domain name
  type VirtualHostFromDomainExtra a :: *

  -- | to retrieve virtual host info by looking up domain name
  virtualHostByDomainName :: VirtualHostFromDomainExtra a -- ^ extra info
                          -> Text                     -- ^ domain name
                          -> IO (Maybe a)

  -- | Quickly determinate if a domain name is not mapped to any virtual host
  virtualHostExcludeDomainName :: Proxy a
                               -> VirtualHostFromDomainExtra a
                               -> Text
                               -> Bool
  virtualHostExcludeDomainName _ _ _ = False


-- | How a virtual host map to a domain name
class VirtualHostToDomain a where
  type VirtualHostToDomainExtra a :: *

  -- | Get the domain name of a virtual host
  virtualHostToDomainName :: VirtualHostToDomainExtra a
                          -> a
                          -> IO (Maybe Text)


-- | Put information about virtual host of current request
-- into vault of it. This is the key.
-- The vaule in stored in vault is the virtual host and its domain name.
type VirtualHostVaultKey s = V.Key (s, Text)

class HasVirtualHostVaultKey s a where
  getVirtualHostVaultKey :: a -> VirtualHostVaultKey s

instance HasVirtualHostVaultKey s (VirtualHostVaultKey s) where
  getVirtualHostVaultKey = id


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
nameBasedVirtualHostMiddleware :: forall s. (VirtualHostPath s, VirtualHostFromDomain s)
                               => VirtualHostVaultKey s
                               -> VirtualHostFromDomainExtra s
                               -> Middleware
nameBasedVirtualHostMiddleware k vh_extra app req respond_func = do
  m_req2 <- runMaybeT $ do
    guard $ not $ virtualHostPathShouldNotPrefix proxy_s (pathInfo req)
    virtual_domain <- MaybeT $ return m_virtual_domain

    svs <- MaybeT $ virtualHostByDomainName vh_extra virtual_domain

    let path_prefix = virtualHostPathPrefix svs
    let new_path_info = path_prefix <> pathInfo req

    return $ req { pathInfo = new_path_info
                 , vault = V.insert k (svs, virtual_domain) (vault req)
                 }

  app (fromMaybe req m_req2) respond_func

  where
    proxy_s = Proxy :: Proxy s

    m_virtual_domain = do
      host_name <- fmap decodeUtf8 $ requestHeaderHost req
      let host_domain = toLower $ takeWhile (/= ':') host_name

      guard $ not $ virtualHostExcludeDomainName proxy_s vh_extra host_domain

      return host_domain


-- | To be used when implementing 'approot' method of Yesod class.
-- Like this: approot = ApprootRequest (virtualHostAppRoot (Proxy :: Proxy XXX))
-- With this function, routes of virtual host will be rendered correct domain name.
--
-- CAUTION: Works with nameBasedVirtualHostMiddleware
virtualHostAppRoot :: forall s master. (HasVirtualHostVaultKey s master, HasMasterApproot master)
                   => Proxy s
                   -> master
                   -> Request
                   -> Text
virtualHostAppRoot _ foundation req =
  fromMaybe master_approot $ do
    (_ :: s, domain) <- V.lookup k (vault req)
    fmap fromString $ replaceAppRootDomain (unpack domain) (unpack master_approot)
  where
    k              = getVirtualHostVaultKey foundation
    master_approot = getMasterApproot foundation


replaceAppRootDomain :: String  -- ^ new domain
                     -> String  -- ^ original app root
                     -> Maybe String
replaceAppRootDomain domain master_approot = do
    uri <- parseURI master_approot
    uri_auth <- uriAuthority uri
    return $ flip (uriToString id) "" $ uri { uriAuthority = Just (uri_auth { uriRegName = domain }) }


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


-- | Try to render a route using virtual host domain name
-- Try using url in the form of virtual host,
-- fall back to normal form (master host).
virtualHostRenderUrlParams :: forall s m.
                              ( VirtualHostToDomain s, VirtualHostPath s
                              , HasMasterApproot (HandlerSite m), MonadHandler m
                              , RenderRoute (HandlerSite m)
                              )
                           => Proxy s
                           -> VirtualHostToDomainExtra s
                           -> Route (HandlerSite m)
                           -> [(Text, Text)]
                           -> m Text
virtualHostRenderUrlParams _ to_extra r params = do
  foundation <- getYesod
  let master_approot = getMasterApproot foundation
      def_url        = joinPath dummy_yesod master_approot pps (params0 <> params)

  fmap (decodeUtf8 . toByteString) $
    case virtualHostFromPath pps of
      Nothing -> -- not a virtual host path
        return def_url

      Just (vh :: s, pps2) ->  do
        -- it is a virtual host path
        m_vh_domain <- liftIO $ virtualHostToDomainName to_extra vh
        let m_app_root = do
                domain <- m_vh_domain
                replaceAppRootDomain (unpack domain) (unpack master_approot)

        case m_app_root of
          Nothing -> return def_url

          Just app_root -> do
            return $ joinPath dummy_yesod (fromString app_root) pps2 (params0 <> params)

  where
    dummy_yesod    = LiteApp $ \_ _ -> Nothing
    (pps, params0) = renderRoute r
