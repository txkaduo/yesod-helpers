{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.Form where

import Prelude
import Yesod

import qualified Data.Text.Encoding         as TE
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Aeson.Types           as A

import Data.Text                            (Text)
import Data.Maybe                           (catMaybes)
import Text.Blaze.Renderer.Utf8             (renderMarkup)
import Control.Monad                        (liftM)
import Control.Applicative                  (Applicative, pure)
import Text.Parsec                          (parse)

import Yesod.Helpers.Parsec

nameIdToFs :: Text -> Text -> FieldSettings site
nameIdToFs name idName = FieldSettings "" Nothing (Just idName) (Just name) []

nameToFs :: Text -> FieldSettings site
nameToFs name = FieldSettings "" Nothing Nothing (Just name) []

labelNameToFs :: RenderMessage site message => message -> Text -> FieldSettings site
labelNameToFs label name = FieldSettings
                    (SomeMessage label)
                    Nothing             -- tooltip
                    Nothing             -- id
                    (Just name)
                    []


minimialLayoutBody :: Yesod site => WidgetT site IO () -> HandlerT site IO Html
minimialLayoutBody widget = do
    pc <- widgetToPageContent widget
    giveUrlRenderer $ [hamlet|^{pageBody pc}|]

-- | 把 form 的 html 代码用 json 打包返回
jsonOutputForm :: Yesod site => WidgetT site IO () -> HandlerT site IO Value
jsonOutputForm = jsonOutputFormMsg (Nothing :: Maybe Text)

-- | 同上，只是增加了 message 字段
jsonOutputFormMsg :: (Yesod site, RenderMessage site message) =>
    Maybe message -> WidgetT site IO () -> HandlerT site IO Value
jsonOutputFormMsg m_msg formWidget = do
    body <- liftM (TE.decodeUtf8 . LB.toStrict . renderMarkup)
                            (minimialLayoutBody formWidget)
    mr <- getMessageRender
    return $ object $ catMaybes $
                    [ Just $ "body" .= body
                    , fmap (\msg -> "message" .= mr msg) m_msg
                    ]

type ShowFormPage site = WidgetT site IO () -> Enctype -> HandlerT site IO Html

jsonOrHtmlOutputForm :: Yesod site =>
    ShowFormPage site
    -> WidgetT site IO ()
    -> Enctype
    -> [A.Pair]
    -> HandlerT site IO TypedContent
jsonOrHtmlOutputForm show_form formWidget formEnctype other_data = do
    selectRep $ do
        provideRep $ show_form formWidget formEnctype
        provideRep $ do
            js_form <- jsonOutputForm formWidget
            return $ object $ ("form_body" .= js_form) : other_data


-- | the Data.Traversable.traverse function for FormResult
traverseFormResult :: Applicative m => (a -> m b) -> FormResult a -> m (FormResult b)
traverseFormResult f (FormSuccess x)    = fmap FormSuccess $ f x
traverseFormResult _ (FormFailure e)    = pure $ FormFailure e
traverseFormResult _ FormMissing        = pure FormMissing


simpleEncodedField ::
    (SimpleStringRep a, Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    (String -> msg)     -- ^ a function to generate a error message
    -> Field m a
simpleEncodedField mk_msg = checkMMap f (T.pack . simpleEncode) textField
    where
        f t = case parse simpleParser "" t of
                Left err -> return $ Left $ mk_msg $ show err
                Right x -> return $ Right x


simpleEncodedOptionList ::
    (SimpleStringRep a, Enum a, Bounded a) =>
    (a -> Text)     -- ^ to render value to display
    -> OptionList a
simpleEncodedOptionList render = mkOptionList $ map f [minBound .. maxBound]
    where
        f x = Option (render x) x (T.pack $ simpleEncode x)
