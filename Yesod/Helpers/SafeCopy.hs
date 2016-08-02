{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Yesod.Helpers.SafeCopy where

import qualified Data.Text                  as T
import qualified Data.Aeson                 as Aeson
import Data.SafeCopy
import Data.Typeable                        (Typeable)
import Yesod hiding (get)
import Language.Haskell.TH
import Control.Monad.State hiding (get, put)
import Control.Monad.Reader

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative                  ((<$>), (<*>))
#endif
import Data.Serialize                       (Get, Put, get, put)
import Text.Parsec                          (parse)
import Data.Time                            ( UTCTime, NominalDiffTime
                                            , addUTCTime, getCurrentTime
                                            )
import Data.Default                         (Default, def)
import Data.Word                            (Word8)
import Control.DeepSeq                      (NFData(..))

import Yesod.Helpers.Parsec


#if MIN_VERSION_template_haskell(2, 11, 0)
#define NO_OVERLAP Nothing
#else
#define NO_OVERLAP
#endif


newtype SafeCopyJsonVal = SafeCopyJsonVal { unSafeCopyJsonVal :: Aeson.Value }
                            deriving (Eq, Show)

instance SafeCopy SafeCopyJsonVal where
    putCopy (SafeCopyJsonVal v) = contain $ safePut $ Aeson.encode v
    getCopy = contain $ do
                bs <- safeGet
                either fail (return . SafeCopyJsonVal) $ Aeson.eitherDecode' bs

instance NFData SafeCopyJsonVal where
    rnf (SafeCopyJsonVal x) = rnf x


newtype SafeCopyId val = SafeCopyId { unSafeCopyId :: Key val }
                        deriving (Typeable)

deriving instance (Eq (Key val)) => Eq (SafeCopyId val)
deriving instance (Ord (Key val)) => Ord (SafeCopyId val)
deriving instance (Show (Key val)) => Show (SafeCopyId val)

instance (NFData (Key val)) => NFData (SafeCopyId val) where
    rnf (SafeCopyId x) = rnf x

instance PersistEntity val => SafeCopy (SafeCopyId val) where
    putCopy (SafeCopyId k) = contain $ putCopySafeCopyInside k

    getCopy = contain $ SafeCopyId <$> getCopySafeCopyInside

putCopySafeCopyInside :: (PersistField (Key val)) => Key val -> Put
putCopySafeCopyInside k =
    case toPersistValue k of
        PersistInt64 x      -> put (1 :: Word8) >> put x
        PersistObjectId x   -> put (2 :: Word8) >> put x
        PersistByteString x -> put (3 :: Word8) >> put x
        PersistDbSpecific x -> put (4 :: Word8) >> put x
        _                   -> error "unexpected PersistValue in Key"

getCopySafeCopyInside :: (PersistField (Key val)) => Get (Key val)
getCopySafeCopyInside = do
    (fromPersistValue <$> get_pv) >>= either (fail . T.unpack) return
    where
        get_pv = do
            x <- get
            case (x :: Word8) of
                1 -> PersistInt64 <$> get
                2 -> PersistObjectId <$> get
                3 -> PersistByteString <$> get
                4 -> PersistDbSpecific <$> get
                _ -> fail $ "unexpected/unknown PersistValue type tag: "
                                ++ show x

putCopyAnyId :: PersistField (Key val) => Key val -> Contained Put
putCopyAnyId = contain . putCopySafeCopyInside

getCopyAnyId :: PersistField (Key val) => Contained (Get (Key val))
getCopyAnyId = contain $ getCopySafeCopyInside

putCopySimpleEncoded :: SimpleStringRep a => a -> Contained Put
putCopySimpleEncoded = contain . put . simpleEncode

getCopySimpleEncodedInside :: SimpleStringRep a => Get a
getCopySimpleEncodedInside = do
    s <- get
    case parse simpleParser "" (s :: String) of
        Left _ -> mzero
        Right x -> return x

getCopySimpleEncoded :: SimpleStringRep a => Contained (Get a)
getCopySimpleEncoded = contain getCopySimpleEncodedInside

putCopyAnyEntity :: (SafeCopy (Key val), SafeCopy val) => Entity val -> Contained Put
putCopyAnyEntity x = contain $ safePut (entityKey x, entityVal x)

getCopyAnyEntity ::
    (PersistEntity val, SafeCopy (Key val), SafeCopy val) =>
    Contained (Get (Entity val))
getCopyAnyEntity = contain $ fmap (uncurry Entity) safeGet


class TimedDefault a where
    defTimed :: UTCTime -> a

data TimeTagged a = TimeTagged {
                        _ttTime      :: !UTCTime
                        , _unTimeTag :: !a
                    }
                    deriving (Typeable)

instance NFData a => NFData (TimeTagged a) where
    rnf (TimeTagged t x) = rnf t `seq` rnf x

instance Functor TimeTagged where
    fmap f (TimeTagged t x) = TimeTagged t (f x)

instance Default a => TimedDefault (TimeTagged a) where
    defTimed now = TimeTagged now def

initTimedDefault :: TimedDefault a => IO a
initTimedDefault = do
    now <- getCurrentTime
    return $ defTimed now

type MTT a = Maybe (TimeTagged a)

-- provide some lens
-- ttTime :: Lens' (TimeTagged a) UTCTime
ttTime :: Functor f => (UTCTime -> f UTCTime) -> TimeTagged a -> f (TimeTagged a)
ttTime f (TimeTagged t x) = fmap (\t' -> TimeTagged t' x) (f t)

-- unTimeTag :: Lens (TimeTagged a) (TimeTagged b) a b
unTimeTag :: Functor f => (a -> f b) -> TimeTagged a -> f (TimeTagged b)
unTimeTag f (TimeTagged t x) = fmap (TimeTagged t) (f x)

instance SafeCopy a => SafeCopy (TimeTagged a) where
    putCopy x = contain $ do
                safePut $ _ttTime x
                safePut $ _unTimeTag x

    getCopy = contain $ TimeTagged <$> safeGet <*> safeGet

expiryCheckTimeTagged :: NominalDiffTime -> UTCTime -> TimeTagged a -> (Maybe a)
expiryCheckTimeTagged ttl now tt =
    let t = addUTCTime ttl (_ttTime tt)
    in if t >= now
        then Just $ _unTimeTag tt
        else Nothing

-- | like expiryCheckTimeTagged, but specify TTL in Int
expiryCheckTimeTaggedI :: Int -> UTCTime -> TimeTagged a -> (Maybe a)
expiryCheckTimeTaggedI ttl = expiryCheckTimeTagged (fromIntegral ttl)



updateTimeTagged :: MonadState s m => (TimeTagged a -> s -> s) -> UTCTime -> a -> m ()
updateTimeTagged f now = modify . f . TimeTagged now

queryTimeTagged :: MonadReader r m =>
    (r -> TimeTagged a) -> NominalDiffTime -> UTCTime -> m (Maybe a)
queryTimeTagged f ttl now =
    liftM (expiryCheckTimeTagged ttl now . f) ask

-- | like queryTimeTagged, but specify TTL in Int
queryTimeTaggedI :: MonadReader r m =>
    (r -> TimeTagged a) -> Int -> UTCTime -> m (Maybe a)
queryTimeTaggedI f ttl = queryTimeTagged f (fromIntegral ttl)

queryTimeTaggedMaybe :: MonadReader r m =>
    (r -> MTT a) -> NominalDiffTime -> UTCTime -> m (Maybe a)
queryTimeTaggedMaybe f ttl now =
    liftM (join . (fmap $ expiryCheckTimeTagged ttl now) . f) ask

queryTimeTaggedMaybeI :: MonadReader r m =>
    (r -> MTT a) -> Int -> UTCTime -> m (Maybe a)
queryTimeTaggedMaybeI f ttl = queryTimeTaggedMaybe f (fromIntegral ttl)


--------------------------------------------------------------------------------

deriveSafeCopyAnyId :: Name -> Q [Dec]
deriveSafeCopyAnyId name = do
    pcp <- [| putCopyAnyId |]
    gcp <- [| getCopyAnyId |]
    return
        [ safeCopyInstanceD (ConT name)
            [ FunD 'putCopy
                [ Clause [] (NormalB pcp) []
                ]
            , FunD 'getCopy
                [ Clause [] (NormalB gcp) []
                ]
            ]
        ]

deriveSafeCopyEntity :: Name -> Q [Dec]
deriveSafeCopyEntity name = do
    pcp <- [| putCopyAnyEntity |]
    gcp <- [| getCopyAnyEntity |]
    return
        [ safeCopyInstanceD (ConT ''Entity `AppT` ConT name)
            [ FunD 'putCopy
                [ Clause [] (NormalB pcp) []
                ]
            , FunD 'getCopy
                [ Clause [] (NormalB gcp) []
                ]
            ]
        ]

deriveSafeCopySimpleEncoded :: Name -> Q [Dec]
deriveSafeCopySimpleEncoded name = do
    pcp <- [| putCopySimpleEncoded |]
    gcp <- [| getCopySimpleEncoded |]
    return
        [ safeCopyInstanceD (ConT name)
            [ FunD 'putCopy
                [ Clause [] (NormalB pcp) []
                ]
            , FunD 'getCopy
                [ Clause [] (NormalB gcp) []
                ]
            ]
        ]

safeCopyInstanceD :: Type -> [Dec] -> Dec
safeCopyInstanceD typ =
    InstanceD NO_OVERLAP [] (ConT ''SafeCopy `AppT` typ)
