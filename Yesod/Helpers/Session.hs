{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.Session where

import Prelude
import Control.Applicative
import Yesod
import Data.String                          (fromString)
import Data.ByteString                      (ByteString)
import qualified Data.ByteString            as B
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
                            Just n | not (B.null n)   -> ck { setCookieDomain = Just n }
                            _                       -> ck
        add_name ck = case sessCookieSettingsName settings of
                            Just n | not (B.null n)   -> ck { setCookieName = n }
                            _                       -> ck
        add_path ck = case sessCookieSettingsPath settings of
                            Just n | not (B.null n)   -> ck { setCookiePath = Just n }
                            _                       -> ck
        add_max_age ck = case sessCookieSettingsMaxAge settings of
                            Just dt | dt > 0    -> ck { setCookieMaxAge = Just dt }
                                -- 这个逻辑是为了方便可以从环境变量输入一个值(如0)来代表 Nothing
                            _                   -> ck
