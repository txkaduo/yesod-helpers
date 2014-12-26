{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Yesod.Helpers.Form where

import Prelude
import Yesod

#if MIN_VERSION_yesod_form(1, 3, 8)
import Yesod.Form.Bootstrap3                ( renderBootstrap3
                                            , BootstrapFormLayout(BootstrapBasicForm)
                                            )
#endif
import Yesod.Core.Types                     (fileSourceRaw)

import qualified Data.Text.Encoding         as TE
import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString            as B
import qualified Data.Aeson.Types           as A
import qualified Codec.Archive.Zip          as Zip
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Data.Conduit.Attoparsec    as CA
import qualified Data.Attoparsec.ByteString as Atto

import Data.Typeable                        (Typeable)
import Control.Exception                    (Exception)
import Control.Monad.RWS.Lazy               (RWST)
import Text.Blaze                           (Markup)
import Data.Conduit                         (($$), Conduit, yield, await, ($=))
import Control.Monad.Trans.Resource         (runResourceT)
import Control.Monad.Trans.Maybe            (runMaybeT)
import Control.Monad                        (mzero, when)
import Data.Conduit.Binary                  (sourceLbs, sinkLbs)

import Data.Text                            (Text)
import Data.Maybe                           (catMaybes)
import Text.Blaze.Renderer.Utf8             (renderMarkup)
import Text.Blaze.Internal                  (MarkupM(Empty))
import Control.Monad                        (liftM, void, forM)
import Control.Monad.Catch                  (catch, throwM, MonadCatch, MonadThrow)
import Control.Applicative                  (Applicative, pure, (<|>))
import Text.Parsec                          (parse, sepEndBy, many1, space, newline
                                            , eof, skipMany)
import Control.Monad.Trans.Except           (runExceptT, throwE, ExceptT(..))
import Data.Aeson.Types                     (parseEither)
import Data.Yaml                            (decodeEither)

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
#if MIN_VERSION_yesod_core(1, 2, 20)
    withUrlRenderer
#else
    giveUrlRenderer
#endif
        $ [hamlet|^{pageBody pc}|]

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
        provideRep $ do
            js_form <- jsonOutputForm formWidget
            return $ object $ ("form_body" .= js_form) : other_data
        provideRep $ show_form


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


entityField ::
    ( RenderMessage site msg
    , RenderMessage site FormMessage
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistStore (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistStore (YesodDB site)
#endif
    , PersistEntity val
    , YesodPersist site
    , PathPiece (Key val)
    ) =>
    msg
    -> msg
    -> Field (HandlerT site IO) (Entity val)
entityField invalid_msg not_found_msg =
    checkMMap f (toPathPiece . entityKey) strippedTextField
    where
        f t = runExceptT $ do
            k <- maybe (throwE invalid_msg) return $ fromPathPiece t
            (lift $ runDB $ get k) >>= maybe (throwE not_found_msg) (return . Entity k)


entityUniqueKeyField ::
    ( RenderMessage site msg
    , RenderMessage site FormMessage
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistStore (YesodPersistBackend site)
    , PersistUnique (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistUnique (YesodDB site)
#endif
    , PersistEntity val
    , YesodPersist site
    , PathPiece (Key val)
    ) =>
    (Text -> msg)
    -> (Text -> Unique val)
    -> Field (HandlerT site IO) (Entity val)
entityUniqueKeyField not_found_msg mk_unique =
    checkMMap f (toPathPiece . entityKey) strippedTextField
    where
        f t = runExceptT $ do
                (lift $ runDB $ getBy $ mk_unique t)
                    >>= maybe (throwE $ not_found_msg t) return


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
        f t = case parse (parseToList p_sep p) "" (unTextarea t)
                of
                Left err -> return $ Left $ mk_msg $ show err
                Right x -> return $ Right x

-- | This helper function is for encodedListTextareaField.
-- Logcially, all it do is:
-- skipMany p_sep >> p `sepEndBy` (eof <|> (void $ many1 p_sep))
-- But if 'p' 'p_sep' overlaps, there will be problems.
parseToList :: CharParser b -> CharParser a -> CharParser [a]
parseToList p_sep p = do
    -- if p eats some char of 'p_sep' the following line failed
    x <- skipMany p_sep >> p `sepEndBy` (eof <|> (void $ many1 p_sep))
    eof
    return x

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


-- | a textField with its value automatically stripped before parsing.
strippedTextField :: forall m. (Monad m
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    Field m Text
strippedTextField = stripUpFront textField

-- | a textareaField with its value automatically stripped before parsing.
strippedTextareaField :: forall m. (Monad m
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    Field m Textarea
strippedTextareaField = stripUpFront textareaField

-- | strip the value (as Text from client) before parsing.
stripUpFront :: Field m a -> Field m a
stripUpFront fd = fd { fieldParse = new_parse }
    where
        new_parse = fieldParse fd . map T.strip

-- | check the result by constructing a Unique key,
-- if record matching that Unique key already exists, report the error message.
checkFieldDBUnique ::
    ( YesodPersist site, PersistEntity val
    , RenderMessage site msg
#if MIN_VERSION_persistent(2, 0, 0)
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistUnique (YesodPersistBackend site)
#else
    , PersistUnique (YesodDB site)
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
#endif
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


data BytestringTooLarge = BytestringTooLarge
                        deriving (Show, Typeable)

instance Exception BytestringTooLarge

limitedSizeConduitBs ::
    (MonadThrow m) =>
    Int -> Conduit B.ByteString m B.ByteString
limitedSizeConduitBs max_size = go 0
    where
        go cnt = do
            mx <- await
            case mx of
                Nothing -> return ()
                Just x ->  do
                    let new_cnt = cnt + B.length x
                    if new_cnt > max_size
                        then throwM BytestringTooLarge
                        else yield x >> go new_cnt


-- | catch exception in field's parse functions,
-- report it as an error message.
fieldExceptionToMessage :: forall m e msg a.
    (
      RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , Exception e
    , MonadCatch m
    ) =>
    (e -> msg)           -- ^ make a message when file size exceeds limit
    -> Field m a
    -> Field m a
fieldExceptionToMessage mk_msg f = f { fieldParse  = p }
    where
        p txts fis = runExceptT $ do
            (ExceptT $ fieldParse f txts fis)
                `catch` (\err -> throwE $ SomeMessage $ mk_msg err)


-- | modify fileField to a size-limited one.
limitFileSize ::
    (MonadResource m
    , RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , MonadCatch m
    , Integral i
    ) =>
    msg         -- ^ make a message when file size exceeds limit
    -> i        -- ^ the file size limit
    -> Field m FileInfo
    -> Field m FileInfo
limitFileSize err_msg max_size field =
    let h (_ :: BytestringTooLarge) = err_msg
        c       = limitedSizeConduitBs (fromIntegral max_size)
        f fi    = return
                    (Right $ fi { fileSourceRaw = fileSourceRaw fi $= c }
                        :: Either Text FileInfo
                    )
    in fieldExceptionToMessage h $ checkM f field


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
    lift $
#if MIN_VERSION_yesod_form(1, 3, 8)
        -- renderBootstrap is deprecated, but the new recommended function
        -- is for bootstrap v3.
        -- We assume that bootstrap v3 is used here.
        renderBootstrap3 BootstrapBasicForm
#else
        renderBootstrap
#endif
            aform extra

-- | combines renderBootstrapS and smToForm
renderBootstrapS' :: Monad m =>
    Markup
    -> SMForm m (FormResult a)
    -> MForm m (FormResult a, WidgetT (HandlerSite m) IO ())
renderBootstrapS' extra result = do
    smToForm $ result >>= renderBootstrapS extra

smToForm :: Monad m => SMForm m a -> MForm m a
smToForm = flip SS.evalStateT []


-- | a form input field that accept an uploaded YAML file and parse it
yamlFileField ::
    ( RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , FromJSON a, MonadResource m
    ) =>
    (String -> String -> msg)
                            -- ^ error message when fail to parse Yaml file
                            -- 1st arg: file_name
                            -- 2nd arg: parse error
    -> (String -> a -> A.Parser b)
                            -- ^ the parser
    -> Field m FileInfo
    -> Field m (FileInfo, b)
yamlFileField yaml_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            let file_name = T.unpack $ fileName fi
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            either (throwE . yaml_err file_name) (return . (fi,)) $
                (decodeEither $ LB.toStrict bs)
                    >>= parseEither (p file_name)


-- | a form input field that accept
-- * an uploaded YAML file,
-- * or zip file of many YAML files
-- then parse it
zippedYamlFilesField ::
    ( RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , FromJSON a, MonadResource m
    ) =>
    (String -> msg)
    -> (String -> String -> msg)
                            -- ^ error message when fail to parse Yaml file
                            -- 1st arg: file_name
                            -- 2nd arg: parse error
    -> (String -> a -> A.Parser b)
                            -- ^ the parser
    -> Field m FileInfo
    -> Field m (FileInfo, [b])
zippedYamlFilesField unzip_err yaml_err p file_field =
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            let magic_bytes = LB.take 4 bs
            let is_zip = ct == "application/zip" ||
                            (ct == ct_unknown && magic_bytes `elem` zip_magic_bs)
            if is_zip
                then parse_fi_zip fi bs
                else parse_fi_one fi bs
            where
                ct = fileContentType fi
                ct_unknown = "application/octet-stream"
                zip_magic_bs =  [ LB.pack [ 0x50, 0x4b, 0x03, 0x04 ]
                                , LB.pack [ 0x50, 0x4b, 0x05, 0x06 ]
                                , LB.pack [ 0x50, 0x4b, 0x07, 0x08 ]
                                ]

        parse_fi_one fi bs = do
            let file_name = T.unpack $ fileName fi
            either (throwE . yaml_err file_name) (return . (fi,) . (:[])) $
                (decodeEither $ LB.toStrict bs)
                    >>= parseEither (p file_name)

        parse_fi_zip fi bs = do
            arc <- either (throwE . unzip_err) return $
                            Zip.toArchiveOrFail bs
            fmap (fi,) $ liftM catMaybes $ forM (Zip.zEntries arc) $
                \entry -> runMaybeT $ do
                    let file_bs     = LB.toStrict $ Zip.fromEntry entry
                        file_name   = Zip.eRelativePath entry
                    when (B.length file_bs <= 0) $ mzero
                    lift $ either (throwE . yaml_err file_name) return $
                            decodeEither file_bs >>= parseEither (p file_name)


-- | accept an uploaded file and parse it with an Attoparsec Parser
attoparsecFileField ::
    ( RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , MonadResource m, MonadCatch m
    ) =>
    (String -> String -> msg)
                            -- ^ error message when fail to parse Yaml file
                            -- 1st arg: file_name
                            -- 2nd arg: parse error
    -> (String -> Atto.Parser b)
                            -- ^ the parser
    -> Field m FileInfo
    -> Field m (FileInfo, b)
attoparsecFileField parse_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            let file_name = T.unpack $ fileName fi
            (liftM (fi,) $
                liftIO $ runResourceT $
                    fileSourceRaw fi $$ CA.sinkParser (p file_name)
                ) `catch` (\(e :: CA.ParseError) ->
                                throwE $ parse_err file_name $ show e
                            )


-- | accept an uploaded file and parse it with an Attoparsec Parser
-- If uploaded file is a zip file, parse files in it one by one.
zippedAttoparsecFilesField ::
    ( RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , MonadResource m, MonadCatch m
    ) =>
    (String -> msg)
    -> (String -> String -> msg)
                            -- ^ error message when fail to parse Yaml file
                            -- 1st arg: file_name
                            -- 2nd arg: parse error
    -> (String -> Atto.Parser b)
                            -- ^ the parser
    -> Field m FileInfo
    -> Field m (FileInfo, [b])
zippedAttoparsecFilesField unzip_err parse_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            let magic_bytes = LB.take 4 bs
            let is_zip = ct == "application/zip" ||
                            (ct == ct_unknown && magic_bytes `elem` zip_magic_bs)
            if is_zip
                then parse_fi_zip fi
                else parse_fi_one fi
            where
                ct = fileContentType fi
                ct_unknown = "application/octet-stream"
                zip_magic_bs =  [ LB.pack [ 0x50, 0x4b, 0x03, 0x04 ]
                                , LB.pack [ 0x50, 0x4b, 0x05, 0x06 ]
                                , LB.pack [ 0x50, 0x4b, 0x07, 0x08 ]
                                ]

        parse_fi_one fi = do
            let file_name = T.unpack $ fileName fi
            (liftM ((fi,) . (:[])) $
                liftIO $ runResourceT $
                    fileSourceRaw fi $$ CA.sinkParser (p file_name)
                ) `catch` (\(e :: CA.ParseError) ->
                                throwE $ parse_err file_name $ show e
                            )

        parse_fi_zip fi = do
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            arc <- either (throwE . unzip_err) return $
                            Zip.toArchiveOrFail bs
            fmap (fi,) $ liftM catMaybes $ forM (Zip.zEntries arc) $ \entry -> runMaybeT $ do
                let file_bs     = LB.toStrict $ Zip.fromEntry entry
                    file_name   = Zip.eRelativePath entry
                when (B.length file_bs <= 0) $ mzero
                lift $
                    (liftIO $ runResourceT $
                        sourceLbs (LB.fromStrict file_bs) $$ CA.sinkParser (p file_name)
                    ) `catch` (\(e :: CA.ParseError) ->
                                    throwE $ parse_err file_name $ show e
                                )

