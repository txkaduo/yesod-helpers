module Yesod.Helpers.Auth where

import Prelude
import Yesod
import Yesod.Auth
import Control.Monad.Catch                  (MonadThrow)
import Control.Monad

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
    let get_user =
#if MIN_VERSION_yesod_core(1, 4, 0)
                getAuthEntity
#else
                runDB . get
#endif
    user <- liftHandlerT (get_user user_id)
                >>= maybe (permissionDeniedI $ (T.pack "AuthEntity not found")) return
    f (user_id, user)


yesodAuthEntityDoSub :: (YesodAuthPersist master, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
                    ((AuthId master, AuthEntity master) -> HandlerT site (HandlerT master m) a)
                    -> HandlerT site (HandlerT master m) a
yesodAuthEntityDoSub f = do
    let get_user =
#if MIN_VERSION_yesod_core(1, 4, 0)
                getAuthEntity
#else
                runDB . get
#endif
    user_id <- lift (liftHandlerT maybeAuthId)
                >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
    user <- lift (liftHandlerT $ get_user user_id)
                >>= maybe (permissionDeniedI $ (T.pack "AuthEntity not found")) return
    f (user_id, user)


-- | 用于实现一种简单的身份认证手段：使用 google email 作为用户标识
newtype GoogleEmail = GoogleEmail { unGoogleEmail :: T.Text }
                    deriving (Show, Eq, PathPiece)

-- | used to implement 'authenticate' method of 'YesodAuth' class
authenticateGeImpl :: (MonadHandler m, AuthId master ~ GoogleEmail)
                    => Creds master
                    -> m (AuthenticationResult master)
authenticateGeImpl creds = do
    setSession "authed_gmail" raw_email
    return $ Authenticated $ GoogleEmail raw_email
    where
        raw_email = credsIdent creds

-- | used to implement 'manybeAuthId' method of 'YesodAuth' class
maybeAuthIdGeImpl :: MonadHandler m => m (Maybe GoogleEmail)
maybeAuthIdGeImpl = do
    liftM (fmap GoogleEmail) $ lookupSession "authed_gmail"


eitherGetLoggedInUserId :: YesodAuth master
                        => HandlerT master IO (Either AuthResult (AuthId master))
eitherGetLoggedInUserId = do
    m_uid <- maybeAuthId
    case m_uid of
        Nothing -> return $ Left AuthenticationRequired
        Just uid -> return $ Right uid
