{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
module Yesod.Helpers.LoginUser where

import Prelude
import Yesod

import Data.Typeable                        (Typeable)
import Data.Text                            (Text)
import qualified Data.Text                  as T
import Data.Time                            (TimeZone)
import Control.Monad                        (liftM, join)

import Yesod.Helpers.Json                   (jsonDecodeKey, jsonEncodeKey)

-- | Any logged-in user on Web
-- 本意是要打算支持多种类型用户互相独立地登录
-- 但在 Yesod 提供的机制中，一个 Yesod 实例只有一个登录的 Route，
-- 不能依据 isAuthorized 所测试的 Route 而变。
-- 曾经使用过把要登录的用户类型记录在 session 里，
-- 让负责登录的 Route 取出加以选择。
-- 这种方法并不优雅。
class (PersistEntity u, Typeable u) => LoginUser u where

    -- | 对应此种用户的 session key
    loginIdentSK :: Monad m =>
        m u
        -> Text

    loginIdentToKey :: Monad m =>
        m u
        -> Text       -- ^ 在 session 中保存的用户标识字串
        -> Maybe (Key u)
    loginIdentToKey _ ident = either (const Nothing) Just $ jsonDecodeKey ident

    -- | we often need time-zone to display date/time
    loginUserTimeZone :: u -> TimeZone



getLoggedInUser ::
    (PersistEntityBackend u ~ PersistMonadBackend (YesodDB site)
    , PersistStore (YesodDB site)
    , YesodPersist site
    , LoginUser u
    , Monad n
    )
    => n u -> HandlerT site IO (Maybe (Entity u))
getLoggedInUser mu = do
    maybe_key <- liftM (join . (fmap $ loginIdentToKey mu)) $ getLoggedInIdent mu
    case maybe_key of
        Nothing -> return Nothing
        Just k -> runDB $ liftM (fmap $ Entity k) $ get k

getLoggedInIdent :: (LoginUser u, MonadHandler m, Monad n) => n u -> m (Maybe Text)
getLoggedInIdent mu = do
    let sk = loginIdentSK mu
    lookupSession sk

-- | 在 session 中标记某个用户为登录状态
markLoggedIn :: forall u site . (LoginUser u) => Entity u -> HandlerT site IO ()
markLoggedIn eu = do
    let sk = loginIdentSK ([] :: [u])
    setSession sk $ jsonEncodeKey $ entityKey eu

markLoggedOut :: (LoginUser u, Monad n) => n u -> HandlerT site IO ()
markLoggedOut mu = do
    let sk = loginIdentSK mu
    deleteSession sk


------------------------------------------------------------------------------


