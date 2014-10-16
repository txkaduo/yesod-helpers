{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Helpers.Acid where

import Prelude
import Data.Acid
import Data.Acid.Remote

import Data.Typeable                        (Typeable)
import Control.Monad.IO.Class               (MonadIO, liftIO)
import Control.Concurrent                   (forkIO)
import Control.Monad                        (void)
import Control.Monad.Logger                 (MonadLogger, logError)
import Control.Exception                    (try)
import Data.Monoid                          ((<>))
import Data.String                          (fromString)
import System.Timeout                       (timeout)
import Network                              (HostName, PortID)

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
