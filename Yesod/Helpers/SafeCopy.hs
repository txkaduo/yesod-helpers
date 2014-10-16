{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.SafeCopy where

import Data.SafeCopy
import Yesod
import Language.Haskell.TH
import Control.Monad.State
import Control.Monad.Reader

import Control.Applicative                  ((<$>), (<|>), (<*>))
import Data.Serialize                       (Get, Put)
import Text.Parsec                          (parse)
import Data.Time                            ( UTCTime, NominalDiffTime
                                            , addUTCTime, getCurrentTime
                                            )
import Data.Default                         (Default, def)

import Yesod.Helpers.Parsec

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

    getCopy = contain $ getCopyInside


getCopyInside :: Get (SafeCopyId val)
getCopyInside = SafeCopyId . Key <$> get_pv
    where
        get_pv = (PersistInt64 <$> safeGet)
                <|> (PersistObjectId <$> safeGet)
                <|> (PersistDbSpecific <$> safeGet)
                <|> (PersistByteString <$> safeGet)

putCopyAnyId :: Key val -> Contained Put
putCopyAnyId = putCopy . SafeCopyId

getCopyAnyId :: Contained (Get (Key val))
getCopyAnyId = contain $ fmap unSafeCopyId getCopyInside

putCopySimpleEncoded :: SimpleStringRep a => a -> Contained Put
putCopySimpleEncoded = putCopy . simpleEncode

getCopySimpleEncodedInside :: SimpleStringRep a => Get a
getCopySimpleEncodedInside = do
    s <- safeGet
    case parse simpleParser "" (s :: String) of
        Left _ -> mzero
        Right x -> return x

getCopySimpleEncoded :: SimpleStringRep a => Contained (Get a)
getCopySimpleEncoded = contain getCopySimpleEncodedInside

putCopyAnyEntity :: (SafeCopy (Key val), SafeCopy val) => Entity val -> Contained Put
putCopyAnyEntity x = putCopy (entityKey x, entityVal x)

getCopyAnyEntity :: (SafeCopy (Key val), SafeCopy val) => Contained (Get (Entity val))
getCopyAnyEntity = contain $ fmap (uncurry Entity) safeGet


data TimeTagged a = TimeTagged {
                        ttTime      :: !UTCTime
                        , unTimeTag :: !a
                    }

instance SafeCopy a => SafeCopy (TimeTagged a) where
    putCopy x = contain $ do
                safePut $ ttTime x
                safePut $ unTimeTag x

    getCopy = contain $ TimeTagged <$> safeGet <*> safeGet

expiryCheckTimeTagged :: NominalDiffTime -> UTCTime -> TimeTagged a -> (Maybe a)
expiryCheckTimeTagged ttl now tt =
    let t = addUTCTime ttl (ttTime tt)
    in if t >= now
        then Just $ unTimeTag tt
        else Nothing

-- | like expiryCheckTimeTagged, but specify TTL in Int
expiryCheckTimeTaggedI :: Int -> UTCTime -> TimeTagged a -> (Maybe a)
expiryCheckTimeTaggedI ttl = expiryCheckTimeTagged (fromIntegral ttl)

initTimeTaggedDef :: Default a => IO (TimeTagged a)
initTimeTaggedDef = do
    now <- getCurrentTime
    return $ TimeTagged now def


updateTimeTagged :: MonadState s m => (TimeTagged a -> s -> s) -> UTCTime -> a -> m ()
updateTimeTagged f now = modify . f . TimeTagged now

queryTimeTagged :: MonadReader r m =>
    (r -> TimeTagged a) -> NominalDiffTime -> UTCTime -> m (Maybe a)
queryTimeTagged f ttl now =
    liftM (expiryCheckTimeTagged ttl now . f) ask

-- | like queryTimeTagged, but specify TTL in Int
queryTimeTaggedI :: MonadReader r m =>
    (r -> TimeTagged a) -> Int -> UTCTime -> m (Maybe a)
queryTimeTaggedI f ttl now =
    liftM (expiryCheckTimeTagged (fromIntegral ttl) now . f) ask


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

safeCopyInstanceD :: Type -> [Dec] -> Dec
safeCopyInstanceD typ =
    InstanceD [] (ConT ''SafeCopy `AppT` typ)
