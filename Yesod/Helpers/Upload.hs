module Yesod.Helpers.Upload where

import Prelude
import Yesod

import Control.Monad                        (liftM)
import Yesod.Core.Types                     (fileSourceRaw)
import Data.Conduit                         (($=), ($$))
import qualified Data.Conduit.Binary        as CB
import qualified Data.ByteString.Lazy       as LB


-- | modify the fileSourceRaw member,
-- to restrict maximum bytes that are allowed to read from.
fiRestrictSourceSize :: Int -> FileInfo -> FileInfo
fiRestrictSourceSize max_size fi = fi { fileSourceRaw = new_source }
    where
        new_source = fileSourceRaw fi $= CB.isolate max_size


-- | read all content from source in FileInfo,
-- restricted by a maximum byte count,
-- if content length exceeds that limit, return Nothing.
fiReadLimited :: (MonadResource m) =>
    Int           -- ^ maximum bytes allowed
    -> FileInfo
    -> m (Maybe LB.ByteString)
fiReadLimited max_size fi = do
    let fi' = fiRestrictSourceSize (max_size + 1) fi
    lbs <- fiReadUnlimited fi'
    if LB.length lbs > fromIntegral max_size
        then return Nothing
        else return $ Just lbs


fiReadUnlimited :: (MonadResource m) =>
    FileInfo -> m LB.ByteString
fiReadUnlimited fi = fileSource fi $$ CB.sinkLbs

