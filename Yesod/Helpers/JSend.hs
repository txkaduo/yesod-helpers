{-|
 Tools for output JSON message in JSend
-}
module Yesod.Helpers.JSend where

import ClassyPrelude.Yesod
import Control.Monad.Writer (Writer)
import Data.Monoid (Endo)
import Text.Julius (rawJS, Javascript, toJavascript)


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
    toTypedContent = toTypedContent . toJSON

instance HasContentType JSendMsg where
    getContentType _ = getContentType (Nothing :: Maybe Value)


-- | One way to pack JSendMsg to a JSONP message
jsendToJsonp :: Text -> JSendMsg -> Javascript
jsendToJsonp callback jmsg =
  toJavascript $ rawJS $ renderJavascriptUrl dummy_render $ jsendToJsonpU callback jmsg
    where dummy_render _ _ = error "jsendToJsonp: should never reach here"


jsendToJsonpU :: Text -> JSendMsg -> JavascriptUrl url
jsendToJsonpU callback (JSendSuccess v)             = [julius| #{rawJS callback}([#{v}]); |]
jsendToJsonpU callback (JSendFail v)                = [julius| #{rawJS callback}([undefined, #{v}]); |]
jsendToJsonpU callback (JSendError msg code m_data) = [julius| #{rawJS callback}([undefined, #{toJSON msg}, #{toJSON code}, #{toJSON m_data}]); |]


-- | Use this instead of `provideRep`:
-- to provide both a jsend and a jsonp response
provideRepJsendAndJsonp :: (MonadHandler m)
                        => m JSendMsg
                        -> Writer (Endo [ProvidedRep m]) ()
provideRepJsendAndJsonp get_jmsg = do
  provideRep get_jmsg
  provideRep f
  where
    f = do callback <- fmap (fromMaybe "callback") $ lookupGetParam "callback"
           fmap (jsendToJsonp callback) $ get_jmsg


-- vim: set foldmethod=marker:
