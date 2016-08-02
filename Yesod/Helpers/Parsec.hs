{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
module Yesod.Helpers.Parsec
    ( module Yesod.Helpers.Parsec
    , module Text.Parsec.TX.Utils
    )
    where

import Prelude
import Yesod
import Language.Haskell.TH
import Control.Monad
import Data.Text                            (Text)

import Text.Parsec.TX.Utils

derivePathPieceS :: String -> Q [Dec]
derivePathPieceS s = do
    to_pathpiece <- [| toPathPiece . simpleEncode |]
    from_pathpiece <- [| join . fmap (parseWithCharParserMaybe simpleParser) . (fromPathPiece :: Text -> Maybe Text) |]
    return
        [ pathPieceInstanceD (ConT $ mkName s)
            [ FunD 'toPathPiece
                [ Clause [] (NormalB to_pathpiece) []
                ]
            , FunD 'fromPathPiece
                [ Clause [] (NormalB from_pathpiece) []
                ]
            ]
        ]


#if MIN_VERSION_template_haskell(2, 11, 0)
#define NO_OVERLAP Nothing
#else
#define NO_OVERLAP
#endif

pathPieceInstanceD :: Type -> [Dec] -> Dec
pathPieceInstanceD typ =
    InstanceD NO_OVERLAP [] (ConT ''PathPiece `AppT` typ)
