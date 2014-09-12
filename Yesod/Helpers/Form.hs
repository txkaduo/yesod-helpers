{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module Yesod.Helpers.Form where

import Prelude
import Yesod

import qualified Data.Text.Encoding         as TE
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Aeson.Types           as A
import qualified Control.Monad.Trans.State.Strict as SS
import Control.Monad.RWS.Lazy               (RWST)
import Text.Blaze                           (Markup)

import Data.Text                            (Text)
import Data.Maybe                           (catMaybes)
import Text.Blaze.Renderer.Utf8             (renderMarkup)
import Text.Blaze.Internal                  (MarkupM(Empty))
import Control.Monad                        (liftM, void)
import Control.Applicative                  (Applicative, pure, (<|>))
import Text.Parsec                          (parse, sepEndBy, many1, space, newline
                                            , eof, skipMany)
import Control.Monad.Trans.Except           (runExceptT, throwE)

import Yesod.Helpers.Parsec
import Yesod.Helpers.Upload                 (fiReadLimited, fiReadUnlimited)

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
    jsonOrHtmlOutputForm' (show_form formWidget formEnctype) formWidget other_data

jsonOrHtmlOutputForm' :: Yesod site =>
    HandlerT site IO Html
    -> WidgetT site IO ()
    -> [A.Pair]
    -> HandlerT site IO TypedContent
jsonOrHtmlOutputForm' show_form formWidget other_data = do
    selectRep $ do
        provideRep $ show_form
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


-- | parse the content in textarea, into a list of values
-- using methods of SimpleStringRep
simpleEncodedListTextareaField ::
    (SimpleStringRep a, Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    (CharParser b, Text)   -- ^ separator: parser and its 'standard' representation
    -> (String -> msg)      -- ^ a function to generate a error message
    -> Field m [a]
simpleEncodedListTextareaField sep_inf mk_msg =
    encodedListTextareaField sep_inf (simpleParser, T.pack . simpleEncode) mk_msg


simpleEncodedOptionList ::
    (SimpleStringRep a, Enum a, Bounded a) =>
    (a -> Text)     -- ^ to render value to display
    -> OptionList a
simpleEncodedOptionList render = mkOptionList $ map f [minBound .. maxBound]
    where
        f x = Option (render x) x (T.pack $ simpleEncode x)


-- | parse the content in textarea, into a list of values
encodedListTextareaField ::
    (Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    (CharParser b, Text)   -- ^ separator: parser and its 'standard' representation
    -> (CharParser a, a -> Text)
                            -- ^ parse a single value and render a single value
    -> (String -> msg)      -- ^ a function to generate a error message
    -> Field m [a]
encodedListTextareaField (p_sep, sep) (p, render) mk_msg =
    checkMMap f (Textarea . T.intercalate sep . map render) textareaField
    where
        f t = case parse
                (skipMany p_sep >> p `sepEndBy` (eof <|> (void $ many1 p_sep)))
                "" (unTextarea t)
                of
                Left err -> return $ Left $ mk_msg $ show err
                Right x -> return $ Right x


-- | parse every line in textarea, each nonempty line parsed as a single value
lineSepListTextareaField ::
    (Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    (CharParser a, a -> Text)
                            -- ^ parse a single value and render a single value
    -> (String -> msg)      -- ^ a function to generate a error message
    -> Field m [a]
lineSepListTextareaField =
    encodedListTextareaField (newline, "\n")


-- | use whitespace to separate strings, and parsed into a list of values
wsSepListTextareaField ::
    (Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    (CharParser a, a -> Text)
                            -- ^ parse a single value and render a single value
    -> (String -> msg)      -- ^ a function to generate a error message
    -> Field m [a]
wsSepListTextareaField =
    encodedListTextareaField (space, "\n")


-- | check the result by constructing a Unique key,
-- if record matching that Unique key already exists, report the error message.
checkFieldDBUnique ::
    ( YesodPersist site, PersistEntity val
    , RenderMessage site msg
    , PersistUnique (YesodDB site)
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    ) =>
    (a -> Unique val)
    -> msg
    -> Field (HandlerT site IO) a
    -> Field (HandlerT site IO) a
checkFieldDBUnique mk_unique msg = checkMMap chk id
    where
        chk t = (runDB $ getBy $ mk_unique t)
                    >>= return . maybe
                            (Right t)
                            (const $ Left msg)


-- | can be used as a placeholder
emptyFieldView :: FieldView site
emptyFieldView = FieldView
                    { fvLabel       = Empty
                    , fvTooltip     = Nothing
                    , fvId          = ""
                    , fvInput       = return ()
                    , fvErrors      = Nothing
                    , fvRequired    = False
                    }

-- | XXX: not a very elegant way to judge whether a FieldView is empty or not
isEmptyFieldView :: FieldView site -> Bool
isEmptyFieldView fv = fvId fv == ""

fvClearErrors :: FieldView site -> FieldView site
fvClearErrors fv = fv { fvErrors = Nothing }

-- | call a function when FormResult is success,
-- otherwise use the default value
caseFormResult :: b -> (a -> b) -> FormResult a -> b
caseFormResult x _ FormMissing      = x
caseFormResult x _ (FormFailure _)  = x
caseFormResult _ f (FormSuccess r)  = f r


-- | whether a FormResult is success and contains expected value.
ifFormResult ::
    (a -> Bool)      -- ^ check value is expected
    -> FormResult a
    -> Bool             -- ^ if result is expected
ifFormResult = caseFormResult False


data SunkFileInfo = SunkFileInfo {
                        sfiFileInfo :: FileInfo
                        , sfiContent :: LB.ByteString
                    }


-- | modify fileField to a size-limited one.
-- result type has been changed to SunkFileInfo.
limitedSizeFileField ::
    (MonadResource m
    , RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    ) =>
    (Int -> msg)        -- ^ make a message when file size exceeds limit
    -> Maybe Int        -- ^ the file size limit
    -> Field m SunkFileInfo
limitedSizeFileField mk_msg m_max_size = checkMMap f sfiFileInfo fileField
    where
        f fi = runExceptT $ do
            liftM (SunkFileInfo fi) $
                case m_max_size of
                    Nothing         -> lift $ fiReadUnlimited fi
                    Just max_size   -> (lift $ fiReadLimited max_size fi)
                                        >>= maybe (throwE $ mk_msg max_size) return

-- | convert a FormResult containing a Either into a vanilla one
-- i.e. convert a Left value into FormFailure.
liftEitherFormResult ::
    (msg -> Text)
    -> FormResult (Either msg a)
    -> FormResult a
liftEitherFormResult render (FormSuccess (Left err))    = FormFailure $ [ render err ]
liftEitherFormResult _      (FormSuccess (Right x))     = FormSuccess x
liftEitherFormResult _      (FormFailure errs)          = FormFailure errs
liftEitherFormResult _      FormMissing                 = FormMissing


-- | the following long type synonym actually says:
-- type SMForm site m a = SS.StateT [FieldView site] (MForm m) a
-- but haskell does not allow partially applied synonym in the above line,
-- we have the expand the synonym manually.
--
-- Usage: With the following helpers (smreq, smopt), all FieldView's are remembered.
-- So usually we don't need to name all FieldView's one by one,
-- which simplify code a little.
type SMForm m a = SS.StateT [FieldView (HandlerSite m)] (RWST (Maybe (Env, FileEnv), HandlerSite m, [Lang]) Enctype Ints m) a

smreq ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    Field m a
    -> FieldSettings site
    -> Maybe a
    -> SMForm m (FormResult a)
smreq field settings initv = do
    (res, view) <- lift $ mreq field settings initv
    SS.modify ( view : )
    return res

smopt ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    Field m a
    -> FieldSettings site
    -> Maybe (Maybe a)
    -> SMForm m (FormResult (Maybe a))
smopt field settings initv = do
    (res, view) <- lift $ mopt field settings initv
    SS.modify ( view : )
    return res


renderBootstrapS :: Monad m =>
    Markup -> FormResult a -> SMForm m (FormResult a, WidgetT (HandlerSite m) IO ())
renderBootstrapS extra result = do
    views <- liftM reverse $ SS.get
    let aform = formToAForm $ return (result, views)
    lift $ renderBootstrap aform extra

smToForm :: Monad m => SMForm m a -> MForm m a
smToForm = flip SS.evalStateT []
