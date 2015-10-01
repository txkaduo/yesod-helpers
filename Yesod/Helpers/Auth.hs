{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.Auth where

import Prelude
import Yesod
import Yesod.Auth
import Control.Monad.Catch                  (MonadThrow)

import qualified Data.Text                  as T


yesodAuthIdDo :: (YesodAuth master, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
                (AuthId master -> HandlerT master m a)
                -> HandlerT master m a
yesodAuthIdDo f = do
    liftHandlerT maybeAuthId
        >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
        >>= f


yesodAuthIdDoSub :: (YesodAuth master, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
                    (AuthId master -> HandlerT site (HandlerT master m) a)
                    -> HandlerT site (HandlerT master m) a
yesodAuthIdDoSub f = do
    (lift $ liftHandlerT maybeAuthId)
        >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
        >>= f


yesodAuthEntityDo :: (YesodAuthPersist master, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
                    ((AuthId master, AuthEntity master) -> HandlerT master m a)
                    -> HandlerT master m a
yesodAuthEntityDo f = do
    user_id <- liftHandlerT maybeAuthId
                >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
    user <- liftHandlerT (getAuthEntity user_id)
                >>= maybe (permissionDeniedI $ (T.pack "AuthEntity not found")) return
    f (user_id, user)


yesodAuthEntityDoSub :: (YesodAuthPersist master, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
                    ((AuthId master, AuthEntity master) -> HandlerT site (HandlerT master m) a)
                    -> HandlerT site (HandlerT master m) a
yesodAuthEntityDoSub f = do
    user_id <- lift (liftHandlerT maybeAuthId)
                >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
    user <- lift (liftHandlerT $ getAuthEntity user_id)
                >>= maybe (permissionDeniedI $ (T.pack "AuthEntity not found")) return
    f (user_id, user)
