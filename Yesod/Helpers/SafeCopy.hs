module Yesod.Helpers.SafeCopy where

import Data.SafeCopy
import Yesod

import Control.Applicative                  ((<$>), (<|>))

newtype SafeCopyId val = SafeCopyId { unSafeCopyId :: Key val }
                        deriving (Eq, Ord, Show)

instance SafeCopy (SafeCopyId val) where
    putCopy (SafeCopyId k) = contain $ do
            case unKey k of
                PersistInt64 x      -> safePut x
                PersistObjectId x   -> safePut x
                PersistByteString x -> safePut x
                PersistDbSpecific x -> safePut x
                _                   -> error "unexpected PersistValue in Key"

    getCopy = contain $ SafeCopyId . Key <$> get_pv
        where
            get_pv = (PersistInt64 <$> safeGet)
                    <|> (PersistObjectId <$> safeGet)
                    <|> (PersistDbSpecific <$> safeGet)
                    <|> (PersistByteString <$> safeGet)
