module Yesod.Helpers.Auth where

import Prelude
import Yesod
import Yesod.Auth

import qualified Data.Text                  as T


yesodAuthIdDo :: YesodAuth master =>
                (AuthId master -> HandlerT master IO a)
                -> HandlerT master IO a
yesodAuthIdDo f = do
    maybeAuthId
        >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
        >>= f


yesodAuthEntityDo :: YesodAuthPersist master =>
                    (AuthEntity master -> HandlerT master IO a)
                    -> HandlerT master IO a
yesodAuthEntityDo f = do
    maybeAuthId
        >>= maybe (permissionDeniedI $ (T.pack "Login Required")) return
        >>= getAuthEntity
        >>= maybe (permissionDeniedI $ (T.pack "AuthEntity not found")) return
        >>= f
