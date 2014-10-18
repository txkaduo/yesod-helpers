{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.Acid
    ( module Yesod.Helpers.Acid
    , AcidState
    , skipAuthenticationCheck, skipAuthenticationPerform
    , sharedSecretCheck, sharedSecretPerform
    ) where

import Prelude
import Data.Acid
import Data.Acid.Remote
import Data.SafeCopy                        (SafeCopy)

import Data.Typeable                        (Typeable)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Concurrent                   (forkIO)
import Control.Applicative                  ((<$>), (<*>))
import Control.Monad                        (void, forever)
import Control.Monad.Logger                 (MonadLogger, logError)
import Control.Exception                    (try)
import Control.Concurrent                   (threadDelay)
import Control.Arrow                        ((&&&))
import Data.Monoid                          ((<>))
import Data.String                          (fromString)
import System.Timeout                       (timeout)
import Network                              (HostName, PortID)
import Data.Aeson                           (withObject, FromJSON, parseJSON
                                            , (.:), (.:?), (.!=)
                                            )
import Data.Streaming.Network               (HostPreference, bindPortTCP)
import Data.Time                            (getCurrentTime, UTCTime)

import Yesod.Helpers.Aeson                  (parseTextByParsec)
import Yesod.Helpers.Parsec                 (parseFileOrNetworkPath)

acidCached ::
    ( MonadIO m
    , QueryEvent eventq, UpdateEvent eventu
    , EventResult eventq ~ Maybe r
    , st ~ EventState eventq
    , st ~ EventState eventu
    ) =>
    eventq
    -> (r -> eventu)
    -> m r
    -> AcidState st
    -> m r
acidCached q u f acid = do
    mr <- liftIO $ query acid q
    case mr of
        Just x  -> return x
        Nothing -> do
            new_r <- f
            _ <- liftIO $ forkIO $ void $ update acid (u new_r)
            return new_r


class AcidCacheArm st f a where
    acidCacheArmed :: a -> f -> AcidState st -> f

-- | This instance says:
-- use a pair of Query and Update to wrap a function,
-- cache its result in acid.
-- retrieve the result from cache if cache is still considered fresh,
-- otherwise call the original function to get fresh result.
instance
    ( MonadIO m
    , QueryEvent eventq, UpdateEvent eventu
    , EventResult eventq ~ Maybe r
    , st ~ EventState eventq
    , st ~ EventState eventu
    ) =>
    AcidCacheArm st (m r) (eventq, r -> eventu)
    where
    acidCacheArmed (q, u) func acid = acidCached q u func acid

instance AcidCacheArm st f a =>
    AcidCacheArm st (a1 -> f) (a1 -> a)
    where
    acidCacheArmed x func acid =
        \x1 -> acidCacheArmed (x x1) (func x1) acid


-- | simple helper: usually we need to provide current time to
-- construct the Query and Update.
acidCacheArmedTTL ::
    ( AcidCacheArm st (m r) (c, c'), MonadIO m) =>
    (UTCTime -> c)
    -> (UTCTime -> c')
    -> m r              -- ^ the function to wrap
    -> AcidState st
    -> m r
acidCacheArmedTTL q u f acid = do
    now <- liftIO getCurrentTime
    acidCacheArmed
        (q &&& u $ now)
        f
        acid

-- | openRemoteState with timeout and log
openRemoteStateTL :: (Typeable st, IsAcidic st, MonadIO m, MonadLogger m) =>
    Int
    -> HostName
    -> PortID
    -> m (Maybe (AcidState st))
openRemoteStateTL ms hostname port = do
    m_acid <- liftIO $ timeout ms $
                try $ openRemoteState skipAuthenticationPerform hostname port
    case m_acid of
        Nothing -> do
                $(logError) $ "cannot open remote acid-state server: timed-out in "
                                <> fromString (show ms)
                                <> " microseconds"
                return Nothing
        Just (Left err) -> do
                $(logError) $ "cannot open remote acid-state server, got exception: "
                                <> (fromString $ show (err :: IOError))
                return Nothing
        Just (Right x) -> return $ Just x


-- | configuration for acid-state
data AcidStateConfig = AcidStateConfig {
                        acidConfigConnect       :: Either FilePath (HostName, PortID)
                                    -- ^ open local or connect to remote acid
                        , acidConfigServeHost   :: HostPreference
                                    -- ^ serve acid state at host
                        , acidConfigPort        :: Maybe Int
                                    -- ^ serve acid state at this port
                                    -- if it is Nothing, no server will be started
                        }
                        deriving (Show)

instance FromJSON AcidStateConfig where
    parseJSON =
        withObject "AcidStateConfig" $ \obj -> do
            AcidStateConfig
                <$> ( obj .: "connect" >>= parseTextByParsec parseFileOrNetworkPath )
                <*> ( fmap fromString $ obj .:? "serve-host" .!= "*4" )
                <*> ( obj .:? "serve-port" )


acidServeOn :: (SafeCopy st) =>
    (CommChannel -> IO Bool)
    -> Int -> HostPreference
    -> AcidState st
    -> IO ()
acidServeOn auth port host acid = do
    socket <- bindPortTCP port host
    acidServer' auth socket acid


acidFlushRepeatly ::
    Int -> AcidState st -> IO ()
acidFlushRepeatly ms acid = forever $ do
    threadDelay ms
    createCheckpoint acid

acidServerByConfig :: (SafeCopy st) =>
    AcidStateConfig
    -> Maybe ((CommChannel -> IO Bool) -> AcidState st -> IO ())
        -- ^ return the IO action if server should be started
acidServerByConfig config =
    fmap (\x auth acid-> acidServeOn auth x host acid) $ acidConfigPort config
    where
        host = acidConfigServeHost config

-- | a network server of acid or repeatly create check point
-- use this as a entry point of background thread
acidServeOrFlushByConfig :: (SafeCopy st) =>
    (CommChannel -> IO Bool)
    -> Int              -- ^ delay time in microseconds
    -> AcidStateConfig
    -> AcidState st
    -> IO ()
        -- ^ return the IO action if server should be started
acidServeOrFlushByConfig auth ms config acid =
    maybe (acidFlushRepeatly ms) ($ auth) (acidServerByConfig config) $ acid

-- | open acid state according to AcidStateConfig
acidOpenByConfig ::
    (SafeCopy st, IsAcidic st, Typeable st, MonadLogger m, MonadIO m) =>
    st
    -> AcidStateConfig
    -> Int
    -> m (Maybe (AcidState st))
acidOpenByConfig s config ms = do
    case acidConfigConnect config of
        Right (host, port)  -> openRemoteStateTL ms host port
        Left fp             -> liftIO $ fmap Just $ openLocalStateFrom fp s
