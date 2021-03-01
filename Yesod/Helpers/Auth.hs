module Yesod.Helpers.Auth where

#if !MIN_VERSION_yesod(1, 6, 0)
import Control.Monad.Trans.Control
import Control.Monad.Catch
#endif

import ClassyPrelude
import Yesod
import Yesod.Auth

import Yesod.Compat


yesodAuthIdDo :: (YesodAuth master, MonadHandler m, master ~ HandlerSite m)
              => (AuthId master -> m a)
              -> m a
yesodAuthIdDo f = do
#if MIN_VERSION_yesod(1, 6, 0)
  liftHandler maybeAuthId
#else
  liftHandlerT maybeAuthId
#endif
      >>= maybe (permissionDeniedI $ asText "Login Required") return
      >>= f


#if !MIN_VERSION_yesod(1, 6, 0)
yesodAuthIdDoSub :: ( YesodAuth master, MonadHandler m, master ~ HandlerSite m, MonadBaseControl IO m
                    , h ~ HandlerT site m
                    )
                 => (AuthId master -> h a)
                 -> h a
yesodAuthIdDoSub f = do
    lift (liftHandlerT maybeAuthId)
        >>= maybe (permissionDeniedI $ asText "Login Required") return
        >>= f
#endif


yesodAuthEntityDo :: (YesodAuthPersist master, MonadHandler m, master ~ HandlerSite m)
                  => ((AuthId master, AuthEntity master) -> m a)
                  -> m a
yesodAuthEntityDo f = do
    user_id <-
#if MIN_VERSION_yesod(1, 6, 0)
      liftHandler maybeAuthId
#else
      liftHandlerT maybeAuthId
#endif
                >>= maybe (permissionDeniedI $ asText "Login Required") return
    let get_user =
#if MIN_VERSION_yesod_core(1, 4, 0)
                getAuthEntity
#else
                runDB . get
#endif
    user <-
#if MIN_VERSION_yesod(1, 6, 0)
      liftHandler (get_user user_id)
#else
      liftHandlerT (get_user user_id)
#endif
                >>= maybe (permissionDeniedI $ asText "AuthEntity not found") return
    f (user_id, user)


#if !MIN_VERSION_yesod(1, 6, 0)
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
                >>= maybe (permissionDeniedI $ asText "Login Required") return
    user <- lift (liftHandlerT $ get_user user_id)
                >>= maybe (permissionDeniedI $ asText "AuthEntity not found") return
    f (user_id, user)
#endif


-- | 用于实现一种简单的身份认证手段：使用 google email 作为用户标识
newtype GoogleEmail = GoogleEmail { unGoogleEmail :: Text }
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


eitherGetLoggedInUserId :: (YesodAuth master, MonadHandler m, HandlerSite m ~ master)
                        => m (Either AuthResult (AuthId master))
eitherGetLoggedInUserId = do
    m_uid <- liftMonadHandler maybeAuthId
    case m_uid of
        Nothing -> return $ Left AuthenticationRequired
        Just uid -> return $ Right uid
