{-|
 Tools for output JSON message in JSend
-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.JSend where

import Yesod
import Data.Text                            (Text)
import Data.Maybe

data JSendMsg = JSendSuccess Value
                | JSendFail Value
                | JSendError Text (Maybe Integer) (Maybe Value)
                    -- ^ message, code, data
                deriving (Show, Read, Eq)


instance ToJSON JSendMsg where
    toJSON (JSendSuccess dat) = object  [ "status" .= ("success" :: Text)
                                        , "data" .= dat
                                        ]
    toJSON (JSendFail dat)  = object [ "status" .= ("fail" :: Text)
                                     , "data" .= dat
                                     ]
    toJSON (JSendError err_msg m_code m_dat) = object $ catMaybes
                                                [ Just $ "status" .= ("error" :: Text)
                                                , Just $ "message" .= err_msg
                                                , fmap ("data" .= ) m_dat
                                                , fmap ("code" .= ) m_code
                                                ]
instance ToContent JSendMsg where
    toContent = toContent . toJSON

instance ToTypedContent JSendMsg where
    toTypedContent = toTypedContent . toTypedContent

instance HasContentType JSendMsg where
    getContentType _ = getContentType (Nothing :: Maybe Value)
