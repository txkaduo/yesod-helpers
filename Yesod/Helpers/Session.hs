module Yesod.Helpers.Session where

import ClassyPrelude.Yesod
import qualified Data.Binary                as Bin
import qualified Data.Binary.Put            as Bin
import qualified Data.Binary.Get            as Bin
import qualified Data.Serialize             as SL
import qualified Web.ClientSession          as CS
import Data.Aeson                           (withObject, (.:?))
import Data.Time                            (DiffTime)
import Web.Cookie


-- | for use in settings
data SessionCookieSettings = SessionCookieSettings
                                { sessCookieSettingsDomain  :: Maybe ByteString
                                , sessCookieSettingsName    :: Maybe ByteString
                                , sessCookieSettingsPath    :: Maybe ByteString
                                , sessCookieSettingsMaxAge  :: Maybe DiffTime
                                }
                                deriving (Show)

instance FromJSON SessionCookieSettings where
    parseJSON = withObject "SessionCookieSettings" $ \o -> do
                    SessionCookieSettings <$> (fmap fromString <$> o .:? "domain")
                                        <*> (fmap fromString <$> o .:? "name")
                                        <*> (fmap fromString <$> o .:? "path")
                                        <*> (fmap (fromIntegral :: Int -> DiffTime) <$> o .:? "max-age")


amendSessionCookie :: SessionCookieSettings -> SetCookie -> SetCookie
amendSessionCookie settings = add_domain . add_name . add_path . add_max_age
    where
        add_domain ck = case sessCookieSettingsDomain settings of
                            Just n | not (null n)   -> ck { setCookieDomain = Just n }
                            _                       -> ck
        add_name ck = case sessCookieSettingsName settings of
                            Just n | not (null n)   -> ck { setCookieName = n }
                            _                       -> ck
        add_path ck = case sessCookieSettingsPath settings of
                            Just n | not (null n)   -> ck { setCookiePath = Just n }
                            _                       -> ck
        add_max_age ck = case sessCookieSettingsMaxAge settings of
                            Just dt | dt > 0    -> ck { setCookieMaxAge = Just dt }
                                -- 这个逻辑是为了方便可以从环境变量输入一个值(如0)来代表 Nothing
                            _                   -> ck


-- | like defaultClientSessionBackend, but add extra param to specify session cookie name
defaultClientSessionBackendCkName :: Int -- ^ minutes
                                  -> FilePath -- ^ key file
                                  -> ByteString
                                  -> IO SessionBackend
defaultClientSessionBackendCkName minutes fp ck_name = do
  key <- CS.getKey fp
  let timeout_sec = fromIntegral (minutes * 60)
  (getCachedDate, _closeDateCacher) <- clientSessionDateCacher timeout_sec
  return $
    SessionBackend {
      sbLoadSession = loadClientSession key getCachedDate ck_name
    }


-- | set session data with cereal's Data.Serialize interface
setSessionCereal :: (MonadHandler m, SL.Serialize a)
                 => Text
                 -> a
                 -> m ()
setSessionCereal n x = setSessionBS n (SL.runPut $ SL.put x)


-- | lookup session data with Data.Serialize interface
lookupSessionCereal :: (MonadHandler m, SL.Serialize a
#if !MIN_VERSION_yesod_core(1, 6, 0)
                       , MonadLogger m
#endif
                       )
                    => Text
                    -> m (Maybe a)
lookupSessionCereal n = do
  m_bs <- lookupSessionBS n
  case m_bs of
    Nothing -> return Nothing
    Just bs -> do
      case SL.runGet SL.get bs of
        Left err -> do $logError $ "Cannot deserialize session data of key '" <> n
                         <> "' : " <> fromString err
                       return Nothing

        Right x -> return x


-- | set session data with Data.Binary interface
setSessionBinary :: (MonadHandler m, Bin.Binary a)
                 => Text
                 -> a
                 -> m ()
setSessionBinary n x = setSessionBS n (toStrict $ Bin.runPut $ Bin.put x)


-- | lookup session data with Data.Binary interface
lookupSessionBinary :: (MonadHandler m, Bin.Binary a
#if !MIN_VERSION_yesod_core(1, 6, 0)
                       , MonadLogger m
#endif
                       )
                    => Text
                    -> m (Maybe a)
lookupSessionBinary n = do
  m_bs <- lookupSessionBS n
  case m_bs of
    Nothing -> return Nothing
    Just bs -> do
      case Bin.runGetOrFail Bin.get (fromStrict bs) of
        Left (_, _, err) -> do $logError $ "Cannot deserialize session data of key '" <> n
                                  <> "' : " <> fromString err
                               return Nothing

        Right (_, _, x)  -> return x
