-- {{{1 language options
{-# LANGUAGE ScopedTypeVariables #-}
-- }}}1
module Yesod.Helpers.Form where

-- {{{1 imports
import ClassyPrelude.Yesod hiding (catch)

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
import qualified Data.ByteString.Char8      as C8
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Base64     as B64
import qualified Data.ByteString.Base64.URL as B64U
import qualified Data.Aeson.Types           as A
import qualified Data.Aeson                 as A
import qualified Codec.Archive.Zip          as Zip
import qualified Control.Monad.Trans.State.Strict as SS
import qualified Data.Conduit.Attoparsec    as CA
import qualified Data.Conduit.List          as CL
import qualified Data.Attoparsec.ByteString as Atto

import Control.Monad.RWS.Lazy               (RWST)
import Text.Blaze                           (Markup)
import Control.Monad.Trans.Resource         (transResourceT)
import Control.Monad.Trans.Maybe            (runMaybeT)
import Control.Arrow                        (right)
import Data.Conduit.Binary                  (sourceLbs, sinkLbs)

import Data.List                            (nub)
import Text.Blaze.Renderer.Utf8             (renderMarkup)
import Text.Blaze.Internal                  (MarkupM(Empty))
import Control.Monad.Catch                  (catch)
import Text.Parsec                          (parse, space, eof, Parsec)
import Control.Monad.Except                 (runExceptT, throwError, ExceptT(..), withExceptT)
import Data.Aeson.Types                     (parseEither)
import Data.Yaml                            (decodeEither)

import qualified Codec.Archive.Smooth.All as AS

import Yesod.Helpers.Message
import Yesod.Helpers.Parsec
import Yesod.Helpers.Utils                  (normalizeChineseMobileNum)
-- }}}1



nameIdToFs :: Text -> Text -> FieldSettings site
nameIdToFs name idName = FieldSettings "" Nothing (Just idName) (Just name) []


nameToFs :: Text -> FieldSettings site
nameToFs name = FieldSettings "" Nothing Nothing (Just name) []


{-# DEPRECATED setPlaceholder "use fsSetPlaceholder instead" #-}
setPlaceholder :: Text -> FieldSettings site -> FieldSettings site
setPlaceholder = fsSetPlaceholder


fsSetAttr :: Text -> Text -> FieldSettings site -> FieldSettings site
fsSetAttr attr_name attr_val (fs@FieldSettings{ fsAttrs = attrs }) =
  fs { fsAttrs = insertMap attr_name attr_val attrs }

fsSetPlaceholder :: Text -> FieldSettings site -> FieldSettings site
fsSetPlaceholder = fsSetAttr "placeholder"


fsSetReadOnly :: FieldSettings site -> FieldSettings site
fsSetReadOnly = fsSetAttr "readonly" "readonly"


labelNameToFs :: RenderMessage site message => message -> Text -> FieldSettings site
-- {{{1
labelNameToFs label name = FieldSettings
                    (SomeMessage label)
                    Nothing             -- tooltip
                    Nothing             -- id
                    (Just name)
                    []
-- }}}1


-- | add 'form-control' CSS class to form input field
fsAddFormControlClass :: FieldSettings site -> FieldSettings site
fsAddFormControlClass = fsAddCssClass "form-control"

-- | add a CSS class to form input field, remove duplicates
fsAddCssClass :: Text -> FieldSettings site -> FieldSettings site
-- {{{1
fsAddCssClass css_cls fs = fs { fsAttrs = new_attrs' }
    where
        classes = fromMaybe "" $ lookup "class" $ fsAttrs fs
        new_classes = T.unwords (nub $ css_cls : T.words classes)
        new_attrs = flip map (fsAttrs fs) $ \(n, v) ->
                        let new_v = if n == "class"
                                       then new_classes
                                       else v
                        in (n, new_v)

        new_attrs' = case lookup "class" new_attrs of
                       Nothing -> ("class", new_classes) : new_attrs
                       Just _ -> new_attrs
-- }}}1


minimialLayoutBody :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
                    WidgetT site IO () -> HandlerT site m Html
-- {{{1
minimialLayoutBody widget = do
    pc <- liftHandlerT $ widgetToPageContent widget
#if MIN_VERSION_yesod_core(1, 2, 20)
    withUrlRenderer
#else
    giveUrlRenderer
#endif
        $ [hamlet|^{pageBody pc}|]
-- }}}1


-- | 把 form 的 html 代码用 json 打包返回
jsonOutputForm :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
    WidgetT site IO () -> HandlerT site m Value
jsonOutputForm = jsonOutputFormMsg (Nothing :: Maybe Text)

-- | 同上，只是增加了 message 字段
jsonOutputFormMsg :: (Yesod site, RenderMessage site message
                    , MonadIO m, MonadThrow m, MonadBaseControl IO m
                    ) =>
                    Maybe message
                    -> WidgetT site IO ()
                    -> HandlerT site m Value
-- {{{1
jsonOutputFormMsg m_msg formWidget = do
    body <- liftM (TE.decodeUtf8 . LB.toStrict . renderMarkup)
                            (minimialLayoutBody formWidget)
    mr <- getMessageRender
    return $ object $ catMaybes $
                    [ Just $ "body" .= body
                    , fmap (\msg -> "message" .= mr msg) m_msg
                    ]
-- }}}1


type ShowFormPage site m = WidgetT site IO () -> Enctype -> HandlerT site m Html

jsonOrHtmlOutputForm :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
    ShowFormPage site m
    -> WidgetT site IO ()
    -> Enctype
    -> [A.Pair]
    -> HandlerT site m TypedContent
jsonOrHtmlOutputForm show_form formWidget formEnctype other_data = do
    jsonOrHtmlOutputForm' (show_form formWidget formEnctype) formWidget other_data

jsonOrHtmlOutputForm' :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
    HandlerT site m Html
    -> WidgetT site IO ()
    -> [A.Pair]
    -> HandlerT site m TypedContent
-- {{{1
jsonOrHtmlOutputForm' show_form formWidget other_data = do
    selectRep $ do
        provideRep $ do
            js_form <- jsonOutputForm formWidget
            return $ object $ ("form_body" .= js_form) : other_data
        provideRep $ show_form
-- }}}1


-- | the Data.Traversable.traverse function for FormResult
traverseFormResult :: forall a b m . Applicative m
                   => (a -> m b)
                   -> FormResult a
                   -> m (FormResult b)
-- {{{1
traverseFormResult f (FormSuccess x)    = fmap FormSuccess $ f x
traverseFormResult _ (FormFailure e)    = pure $ FormFailure e
traverseFormResult _ FormMissing        = pure (FormMissing :: FormResult b)
-- }}}1


-- | Construct a new field that will never fail because of missing value.
-- Useful when user want to control more what should do on missing value.
-- Like report missing value base on other conditions.
neverMissingField :: Monad m
                  => Maybe a    -- ^ put this in result when form value is missing
                  -> Field m a
                  -> Field m (Maybe a)
-- {{{1
neverMissingField val_when_missing field =
  field { fieldParse = fp2, fieldView = fv2 }
  where
    fp2 x y = fmap (right $ Just . maybe val_when_missing Just) $ fieldParse field x y

    fv = fieldView field
    fv2 tid name attrs t_or_res is_req =
      fv tid name attrs t_or_res' is_req
      where
        t_or_res' = case t_or_res of
                      Right Nothing   -> Left "" -- should never reach here
                      Right (Just x)  -> Right x
                      Left t          -> Left t
-- }}}1



{-# DEPRECATED simpleWrappedField "use convertField instead" #-}
-- | useful for newtype type
simpleWrappedField :: forall a b m. Monad m =>
    (a -> b)
    -> (b -> a)
    -> Field m a
    -> Field m b
simpleWrappedField conv conv_back = checkMMap conv' conv_back
    where
        conv' :: a -> m (Either Text b)
        conv' = return . Right . conv


simpleEncodedField ::
    (SimpleStringRep a, Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
    (String -> msg)     -- ^ a function to generate a error message
    -> Field m a
simpleEncodedField mk_msg = simpleEncodedField' mk_msg textField

simpleEncodedField' ::
    (SimpleStringRep a, Monad m
    , RenderMessage (HandlerSite m) msg
    ) =>
    (String -> msg)     -- ^ a function to generate a error message
    -> Field m Text
    -> Field m a
simpleEncodedField' mk_msg old_field = checkMMap f (T.pack . simpleEncode) old_field
    where
        f t = case parse simpleParser "" t of
                Left err -> return $ Left $ mk_msg $ show err
                Right x -> return $ Right x


entityField ::
    ( RenderMessage site msg
    , RenderMessage site FormMessage
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistStore (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistStore (YesodDB site)
#endif
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
            k <- maybe (throwError invalid_msg) return $ fromPathPiece t
            (lift $ runDB $ get k) >>= maybe (throwError not_found_msg) (return . Entity k)

entityKeyField ::
    ( RenderMessage site msg
    , RenderMessage site FormMessage
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistStore (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistStore (YesodDB site)
#endif
    , YesodPersist site
    , PathPiece (Key val)
    ) =>
    msg
    -> msg
    -> Field (HandlerT site IO) (Key val)
entityKeyField invalid_msg not_found_msg =
    checkMMap f toPathPiece strippedTextField
    where
        f t = runExceptT $ do
            k <- maybe (throwError invalid_msg) return $ fromPathPiece t
            (lift $ runDB $ get k) >>= maybe (throwError not_found_msg) (const $ return k)


entityKeyHiddenField ::
    ( RenderMessage site msg
    , RenderMessage site FormMessage
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistStore (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistStore (YesodDB site)
#endif
    , YesodPersist site
    , PathPiece (Key val)
    ) =>
    msg
    -> msg
    -> Field (HandlerT site IO) (Key val)
entityKeyHiddenField invalid_msg not_found_msg =
    checkMMap f toPathPiece hiddenField
    where
        f t = runExceptT $ do
            k <- maybe (throwError invalid_msg) return $ fromPathPiece t
            (lift $ runDB $ get k) >>= maybe (throwError not_found_msg) (const $ return k)


entityUniqueKeyField ::
    ( RenderMessage site msg
    , RenderMessage site FormMessage
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistUnique (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistUnique (YesodDB site)
#endif
    , YesodPersist site
    , PathPiece (Key val)
    ) =>
    (Text -> msg)
    -> (Text -> Unique val)
    -> Field (HandlerT site IO) (Entity val)
-- {{{1
entityUniqueKeyField not_found_msg mk_unique =
    checkMMap f (toPathPiece . entityKey) strippedTextField
    where
        f t = runExceptT $ do
                (lift $ runDB $ getBy $ mk_unique t)
                    >>= maybe (throwError $ not_found_msg t) return
-- }}}1


-- | make a 'mopt' work like 'mreq', by providing a default value
moptDefault :: (site ~ HandlerSite m, MonadHandler m)
             => a   -- ^ the default value
             -> Field m a
             -> FieldSettings site
             -> Maybe a
                    -- ^ initial value of field
             -> MForm m (FormResult a, FieldView site)
moptDefault def_v f fs mv = first (fmap $ fromMaybe def_v) <$> mopt f fs (fmap Just mv)


-- | Keep all text input, don't parse it.
-- Use case: create a dummy select input, fill options with JS.
--           Validate result by 'checkM' or 'checkMMap'
anyTextOptionList :: OptionList Text
anyTextOptionList = OptionList [] (Just . id)


-- | parse the content in textarea, into a list of values
-- using methods of SimpleStringRep
simpleEncodedListTextareaField :: (SimpleStringRep a, Monad m
                                  , RenderMessage (HandlerSite m) msg
                                  , RenderMessage (HandlerSite m) FormMessage
                                  )
                               => (Parsec Text () b, Text)   -- ^ separator: parser and its 'standard' representation
                               -> (String -> msg)      -- ^ a function to generate a error message
                               -> Field m [a]
simpleEncodedListTextareaField sep_inf mk_msg =
    encodedListTextareaField sep_inf (simpleParser, T.pack . simpleEncode) mk_msg


simpleEncodedOptionList ::
    (SimpleEncode a, Enum a, Bounded a) =>
    (a -> Text)     -- ^ to render value to display
    -> OptionList a
simpleEncodedOptionList render = simpleEncodedOptionList' render [minBound .. maxBound]

simpleEncodedOptionList' :: (SimpleEncode a)
                        => (a -> Text)     -- ^ to render value to display
                        -> [a]
                        -> OptionList a
simpleEncodedOptionList' render lst = mkOptionList $ map f lst
    where
        f x = Option (render x) x (T.pack $ simpleEncode x)

-- | parse the content in textarea, into a list of values
encodedListTextareaField :: ( Monad m
                              , RenderMessage (HandlerSite m) msg
                              , RenderMessage (HandlerSite m) FormMessage
                            )
                         => (Parsec Text () b, Text)   -- ^ separator: parser and its 'standard' representation
                         -> (Parsec Text () a, a -> Text)
                                                -- ^ parse a single value and render a single value
                         -> (String -> msg)      -- ^ a function to generate a error message
                         -> Field m [a]
-- {{{1
encodedListTextareaField (p_sep, sep) (p, render) mk_msg =
    checkMMap f (Textarea . T.intercalate sep . map render) textareaField
    where
        f t = case parse (manySepEndBy p_sep p <* eof) "" (unTextarea t) of
                Left err -> return $ Left $ mk_msg $ show err
                Right x -> return $ Right x
-- }}}1


-- | parse every line in textarea, each nonempty line parsed as a single value
lineSepListTextareaField ::
    (Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
      (Parsec Text () a, a -> Text)
                            -- ^ parse a single value and render a single value
    -> (String -> msg)      -- ^ a function to generate a error message
    -> Field m [a]
lineSepListTextareaField = encodedListTextareaField (eol, "\n")


-- | use whitespace to separate strings, and parsed into a list of values
wsSepListTextareaField ::
    (Monad m
    , RenderMessage (HandlerSite m) msg
    , RenderMessage (HandlerSite m) FormMessage
    ) =>
      (Parsec Text () a, a -> Text)
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

-- | input for bytestring, accept base16-encode string
base16Field ::
    ( Monad m, RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    ) =>
    msg
    -> Field m B.ByteString
base16Field msg = checkMMap conv conv_back strippedTextField
    where
        conv_back = T.pack . C8.unpack . B16.encode
        conv t = if B.null leftbs
                    then return $ Right $ okbs
                    else return $ Left msg
            where (okbs, leftbs) = B16.decode (TE.encodeUtf8 t)

base64Field ::
    ( Monad m, RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    ) =>
    msg
    -> Field m B.ByteString
base64Field msg = checkMMap conv conv_back strippedTextField
    where
        conv_back = T.pack . C8.unpack . B64.encode
        conv t = case B64.decode (TE.encodeUtf8 t) of
                    Left _err -> return $ Left msg
                    Right bs  -> return $ Right bs

base64UrlField ::
    ( Monad m, RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    ) =>
    msg
    -> Field m B.ByteString
base64UrlField msg = checkMMap conv conv_back strippedTextField
    where
        conv_back = T.pack . C8.unpack . B64U.encode
        conv t = case B64U.decode (TE.encodeUtf8 t) of
                    Left _err -> return $ Left msg
                    Right bs  -> return $ Right bs

-- | strip the value (as Text from client) before parsing.
stripUpFront :: Field m a -> Field m a
stripUpFront fd = fd { fieldParse = new_parse }
    where
        new_parse = fieldParse fd . map T.strip


checkFieldDBUniqueId ::
    ( YesodPersist site
    , RenderMessage site msg
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistUnique (YesodPersistBackend site)
#else
    , PersistUnique (YesodDB site)
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
#endif
    ) =>
    (a -> Unique val)
    -> msg
    -> Maybe (Key val)
        -- ^ Old value. Don't check DB if result is the same as this.
    -> Field (HandlerT site IO) a
    -> Field (HandlerT site IO) a
-- {{{1
checkFieldDBUniqueId mk_unique msg m_old_id = checkM chk
  where
    chk t = runDB $ runExceptT $ do
      m_rec <- lift $ getBy (mk_unique t)

      case m_rec of
        Nothing -> return ()
        Just (Entity rec_id _) -> do
          case m_old_id of
            Nothing     -> throwError msg
            Just old_id -> unless (old_id == rec_id) $ throwError msg

      return t
-- }}}1


-- | check the result by constructing a Unique key,
-- if record matching that Unique key already exists, report the error message.
{-# DEPRECATED checkFieldDBUnique2 "use checkFieldDBUniqueId instead" #-}
checkFieldDBUnique2 ::
    ( YesodPersist site
    , RenderMessage site msg
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistUnique (YesodPersistBackend site)
#else
    , PersistUnique (YesodDB site)
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
#endif
    , Eq a
    ) =>
    (a -> Unique val)
    -> msg
    -> Maybe a
        -- ^ Old value. Don't check DB if result is the same as this.
    -> Field (HandlerT site IO) a
    -> Field (HandlerT site IO) a
-- {{{1
checkFieldDBUnique2 mk_unique msg m_old_val = checkMMap chk id
    where
        chk t = if Just t == m_old_val
                    then return $ Right t
                    else (runDB $ getBy $ mk_unique t)
                            >>= return . maybe
                                    (Right t)
                                    (const $ Left msg)
-- }}}1


{-# DEPRECATED checkFieldDBUnique "use checkFieldDBUniqueId instead" #-}
checkFieldDBUnique ::
    ( YesodPersist site
    , RenderMessage site msg
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , PersistEntityBackend val ~ YesodPersistBackend site
    , PersistEntity val
#endif

    , PersistUnique (YesodPersistBackend site)
#else
    , PersistUnique (YesodDB site)
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
#endif
    , Eq a
    ) =>
    (a -> Unique val)
    -> msg
    -> Field (HandlerT site IO) a
    -> Field (HandlerT site IO) a
checkFieldDBUnique mk_unique msg = checkFieldDBUnique2 mk_unique msg Nothing


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


-- | 'join' for FormResult.
-- Should be more handly than caseFormResult
joinFormResult :: FormResult (FormResult a) -> FormResult a
-- {{{1
joinFormResult r = case r of
                     FormSuccess x -> x
                     FormMissing   -> FormMissing
                     FormFailure x -> FormFailure x
-- }}}1


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
-- {{{1
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
-- }}}1


-- | catch exception in field's parse functions,
-- report it as an error message.
fieldExceptionToMessage :: forall m e msg a.
    ( RenderMessage (HandlerSite m) msg
    , Exception e
    , MonadCatch m
    ) =>
    (e -> msg)           -- ^ make a message when file size exceeds limit
    -> Field m a
    -> Field m a
fieldExceptionToMessage mk_msg f = f { fieldParse  = p }
    where
        p txts fis = fieldParse f txts fis
                        `catch` (\err -> return $ Left $ SomeMessage $ mk_msg err)


-- | modify fileField to a size-limited one.
limitFileSize :: ( MonadResource m
                 , RenderMessage (HandlerSite m) msg
                 , MonadCatch m
                 , Integral i
                 )
              => msg         -- ^ make a message when file size exceeds limit
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

smreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
      => Field m a
      -> FieldSettings site
      -> Maybe a
      -> SMForm m (FormResult a)
smreq field settings initv = do
    (res, view) <- lift $ mreq field settings initv
    SS.modify ( view : )
    return res

smopt :: ( HandlerSite m ~ site, MonadHandler m)
      => Field m a
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


#if MIN_VERSION_yesod_form(1, 3, 8)
renderBootstrap3S :: Monad m
                  => BootstrapFormLayout
                  -> Markup
                  -> FormResult a
                  -> SMForm m (FormResult a, WidgetT (HandlerSite m) IO ())
renderBootstrap3S layout extra result = do
    views <- liftM reverse $ SS.get
    let aform = formToAForm $ return (result, views)
    lift $ renderBootstrap3 layout aform extra
#endif


-- | combines renderBootstrapS and smToForm
renderBootstrapS' :: Monad m =>
    Markup
    -> SMForm m (FormResult a)
    -> MForm m (FormResult a, WidgetT (HandlerSite m) IO ())
renderBootstrapS' extra result = do
    smToForm $ result >>= renderBootstrapS extra


#if MIN_VERSION_yesod_form(1, 3, 8)
renderBootstrap3S' :: Monad m
                   => BootstrapFormLayout
                   -> Markup
                   -> SMForm m (FormResult a)
                   -> MForm m (FormResult a, WidgetT (HandlerSite m) IO ())
renderBootstrap3S' layout extra result = do
  smToForm $ result >>= renderBootstrap3S layout extra
#endif


smToForm :: Monad m => SMForm m a -> MForm m a
smToForm = flip SS.evalStateT []


jsonTextField :: (FromJSON a, ToJSON a
                 , RenderMessage (HandlerSite m) FormMessage
                 , Monad m
                 )
              => Field m a
-- {{{1
jsonTextField = checkMMap parse_json (toStrict . decodeUtf8 . A.encode) strippedTextField
  where
    parse_json t = case A.eitherDecodeStrict (encodeUtf8 t) of
                     Left err -> return $ Left $ asText $ fromString err
                     Right x -> return $ Right x
-- }}}1


yamlTextareaField ::
    ( RenderMessage (HandlerSite m) FormMessage
    , RenderMessage (HandlerSite m) msg
    , FromJSON a
    , MonadResource m
    ) =>
    (String -> msg)         -- ^ error message when fail to parse Yaml file
    -> (a -> A.Parser b)    -- ^ the parser
    -> Field m (b, Textarea)
-- {{{1
yamlTextareaField yaml_err p =
    checkMMap parse_input snd textareaField
    where
        parse_input t = runExceptT $ do
            either (throwError . yaml_err) (return . (,t)) $ do
                (decodeEither $ TE.encodeUtf8 $ unTextarea t)
                    >>= parseEither p
-- }}}1


-- | a form input field that accept an uploaded YAML file and parse it
yamlFileField :: ( RenderMessage (HandlerSite m) msg
                 , FromJSON a, MonadResource m
                 )
              => (String -> String -> msg)
                            -- ^ error message when fail to parse Yaml file
                            -- 1st arg: file_name
                            -- 2nd arg: parse error
              -> (String -> a -> A.Parser b)
                                      -- ^ the parser
              -> Field m FileInfo
              -> Field m (FileInfo, b)
-- {{{1
yamlFileField yaml_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            let file_name = T.unpack $ fileName fi
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            either (throwError . yaml_err file_name) (return . (fi,)) $
                (decodeEither $ LB.toStrict bs)
                    >>= parseEither (p file_name)
-- }}}1


-- | a form input field that accept
-- * an uploaded YAML file,
-- * or archive file (optionally compressed) of many YAML files
-- then parse it
-- Supported Archive file formats and compression format depend on simple-archive-conduit
archivedYamlFilesField :: ( RenderMessage (HandlerSite m) msg
                          , FromJSON a, MonadResource m
                          , MonadBaseControl IO m
                          )
                       => (String -> msg)
                       -> (String -> String -> msg)
                                              -- ^ error message when fail to parse Yaml file
                                              -- 1st arg: file_name
                                              -- 2nd arg: parse error
                       -> (String -> a -> A.Parser b)
                                              -- ^ the parser
                       -> Field m FileInfo
                       -> Field m (FileInfo, [b])
-- {{{1
archivedYamlFilesField archive_err yaml_err p file_field =
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            fe_list <- runResourceT $ do
                    (rsrc1, m_arc) <- (transPipe (transResourceT liftIO) $ fileSourceRaw fi)
                                        $= AS.autoDecompressByCompressors AS.allKnownDetectiveCompressors
                                        $$+ AS.autoDetectArchive AS.allKnownDetectiveArchives
                    case m_arc of
                        Nothing -> do
                            -- not an archive, treat it as a single file
                            bs <- rsrc1 $$+- sinkLbs
                            let file_name = T.unpack $ fileName fi
                            return $ return $ AS.FileEntry file_name bs

                        Just ax -> do
                            rsrc1
                                $$+- (transPipe
                                        (transResourceT $ withExceptT archive_err)
                                        $ AS.extractFilesByDetectiveArchive ax)
                                =$ CL.consume
            fmap (fi,) $ liftM catMaybes $ forM fe_list $
                \(AS.FileEntry file_name file_bs) -> runMaybeT $ do
                    when (LB.length file_bs <= 0) $ mzero
                    when (not $ ".yml" `isSuffixOf` file_name || ".yaml" `isSuffixOf` file_name)
                        mzero
                    lift $ either (throwError . yaml_err file_name) return $
                            decodeEither (LB.toStrict file_bs) >>= parseEither (p file_name)
-- }}}1


-- | a form input field that accept
-- * an uploaded YAML file,
-- * or zip file of many YAML files
-- then parse it
zippedYamlFilesField :: ( RenderMessage (HandlerSite m) msg
                        , FromJSON a, MonadResource m
                        )
                     => (String -> msg)
                     -> (String -> String -> msg)
                                              -- ^ error message when fail to parse Yaml file
                                              -- 1st arg: file_name
                                              -- 2nd arg: parse error
                     -> (String -> a -> A.Parser b)
                                              -- ^ the parser
                     -> Field m FileInfo
                     -> Field m (FileInfo, [b])
-- {{{1
zippedYamlFilesField unzip_err yaml_err p file_field =
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            let magic_bytes = LB.take 4 bs
            let is_zip = ct == "application/zip" ||
                            ct == "application/x-zip-compressed" ||
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
            either (throwError . yaml_err file_name) (return . (fi,) . (:[])) $
                (decodeEither $ LB.toStrict bs)
                    >>= parseEither (p file_name)

        parse_fi_zip fi bs = do
            arc <- either (throwError . unzip_err) return $
                            Zip.toArchiveOrFail bs
            fmap (fi,) $ liftM catMaybes $ forM (Zip.zEntries arc) $
                \entry -> runMaybeT $ do
                    let file_bs     = LB.toStrict $ Zip.fromEntry entry
                        file_name   = Zip.eRelativePath entry
                    when (B.length file_bs <= 0) $ mzero
                    lift $ either (throwError . yaml_err file_name) return $
                            decodeEither file_bs >>= parseEither (p file_name)
-- }}}1


-- | accept an uploaded file and parse it with an Attoparsec Parser
attoparsecFileField :: ( RenderMessage (HandlerSite m) msg
                       , MonadResource m, MonadCatch m
                       )
                    => (String -> String -> msg)
                            -- ^ error message when fail to parse Yaml file
                            -- 1st arg: file_name
                            -- 2nd arg: parse error
                    -> (String -> Atto.Parser b)
                                            -- ^ the parser
                    -> Field m FileInfo
                    -> Field m (FileInfo, b)
-- {{{1
attoparsecFileField parse_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = do
                        let file_name = T.unpack $ fileName fi
                        (runExceptT $ do
                            liftM (fi,) $
                                liftIO $ runResourceT $
                                    fileSourceRaw fi $$ CA.sinkParser (p file_name)
                            ) `catch` (\(e :: CA.ParseError) ->
                                            return $ Left $ parse_err file_name $ show e
                                        )
-- }}}1


-- | used in archivedFilesField
parseFileInfoAsArchive :: forall e (m :: * -> *).
                          (MonadBase IO m, MonadThrow m, MonadIO m) =>
                          ([Char] -> e)
                          -> FileInfo
                          -> ResourceT (ExceptT e m) [AS.FileEntry]
-- {{{1
parseFileInfoAsArchive archive_err fi = do
    (rsrc1, m_arc) <- (transPipe (transResourceT liftIO) $ fileSourceRaw fi)
                        $= AS.autoDecompressByCompressors AS.allKnownDetectiveCompressors
                        $$+ AS.autoDetectArchive AS.allKnownDetectiveArchives
    case m_arc of
        Nothing -> do
            -- not an archive, treat it as a single file
            bs <- rsrc1 $$+- sinkLbs
            let file_name = T.unpack $ fileName fi
            return $ return $ AS.FileEntry file_name bs

        Just ax -> do
            rsrc1
                $$+- (transPipe
                        (transResourceT $ withExceptT archive_err)
                        $ AS.extractFilesByDetectiveArchive ax)
                =$ CL.consume
-- }}}1


fileInfoToFileEntry :: MonadIO m => FileInfo -> m AS.FileEntry
-- {{{1
fileInfoToFileEntry fi = do
    content <- liftIO $ runResourceT $
                    fileSourceRaw fi $$ sinkLbs
    return $ AS.FileEntry (T.unpack $ fileName fi) content
-- }}}1


archivedFilesField :: ( RenderMessage (HandlerSite m) msg
                      , MonadResource m
                      , MonadBaseControl IO m
                      )
                   => (String -> msg)
                   -> Field m FileInfo
                   -> Field m (FileInfo, [AS.FileEntry])
-- {{{1
archivedFilesField archive_err file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            fe_list <- runResourceT $ parseFileInfoAsArchive archive_err fi
            return (fi, fe_list)
-- }}}1


-- | accept an uploaded file and parse it with an Attoparsec Parser
-- If uploaded file is a zip file, parse files in it one by one.
archivedAttoparsecFilesField :: ( RenderMessage (HandlerSite m) msg
                                , MonadResource m, MonadCatch m
                                , MonadBaseControl IO m
                                )
                             => (String -> msg)
                             -> (String -> String -> msg)
                             -- ^ error message when fail to parse Yaml file
                             -- 1st arg: file_name
                             -- 2nd arg: parse error
                             -> (String -> Atto.Parser b)
                             -- ^ the parser
                             -> Field m FileInfo
                             -> Field m (FileInfo, [b])
-- {{{1
archivedAttoparsecFilesField archive_err parse_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            fe_list <- runResourceT $ parseFileInfoAsArchive archive_err fi
            fmap (fi,) $ liftM catMaybes $ forM fe_list $
                \(AS.FileEntry file_name file_bs) -> runMaybeT $ do
                    when (LB.length file_bs <= 0) $ mzero
                    lift $ ExceptT $
                        (liftM Right $ liftIO $ runResourceT $
                            sourceLbs file_bs $$ CA.sinkParser (p file_name)
                        ) `catch` (\(e :: CA.ParseError) ->
                                        return $ Left $ parse_err file_name $ show e
                                    )
-- }}}1


-- | accept an uploaded file and parse it with an Attoparsec Parser
-- If uploaded file is a zip file, parse files in it one by one.
zippedAttoparsecFilesField :: ( RenderMessage (HandlerSite m) msg
                              , MonadResource m, MonadCatch m
                              )
                           => (String -> msg)
                           -> (String -> String -> msg)
                           -- ^ error message when fail to parse Yaml file
                           -- 1st arg: file_name
                           -- 2nd arg: parse error
                           -> (String -> Atto.Parser b)
                           -- ^ the parser
                           -> Field m FileInfo
                           -> Field m (FileInfo, [b])
-- {{{1
zippedAttoparsecFilesField unzip_err parse_err p file_field = do
    checkMMap parse_sunk fst file_field
    where
        parse_sunk fi = runExceptT $ do
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            let magic_bytes = LB.take 4 bs
            let is_zip = ct == "application/zip" ||
                            ct == "application/x-zip-compressed" ||
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
            ExceptT $
                (liftM Right $ liftM ((fi,) . (:[])) $
                    liftIO $ runResourceT $
                        fileSourceRaw fi $$ CA.sinkParser (p file_name)
                ) `catch` (\(e :: CA.ParseError) ->
                                return $ Left $ parse_err file_name $ show e
                            )

        parse_fi_zip fi = do
            bs <- liftIO $ runResourceT $ fileSourceRaw fi $$ sinkLbs
            arc <- either (throwError . unzip_err) return $
                            Zip.toArchiveOrFail bs
            fmap (fi,) $ liftM catMaybes $ forM (Zip.zEntries arc) $ \entry -> runMaybeT $ do
                let file_bs     = LB.toStrict $ Zip.fromEntry entry
                    file_name   = Zip.eRelativePath entry
                when (B.length file_bs <= 0) $ mzero
                lift $ ExceptT $
                    (liftM Right $ liftIO $ runResourceT $
                        sourceLbs (LB.fromStrict file_bs) $$ CA.sinkParser (p file_name)
                    ) `catch` (\(e :: CA.ParseError) ->
                                    return $ Left $ parse_err file_name $ show e
                                )
-- }}}1


chineseMobileField :: (RenderMessage (HandlerSite m) msg
                    , RenderMessage (HandlerSite m) FormMessage
                    , Monad m
                    ) =>
                    msg
                    -> Field m Text
-- {{{1
chineseMobileField err_msg = checkM chk_mobile strippedTextField
    where
        chk_mobile t = return $ maybe (Left err_msg) Right $ normalizeChineseMobileNum t
-- }}}1


nonNullTextToBoolField :: (Monad m, RenderMessage (HandlerSite m) FormMessage)
                       => Field m Bool
nonNullTextToBoolField = convertField (not . null . T.strip) (\ b -> if b then "1" else "") textField


mustBePositive :: (Num a, Ord a, RenderMessage (HandlerSite m) YHCommonMessage, Monad m)
               => Field m a
               -> Field m a
mustBePositive = checkBool (> 0) MsgFormMsgMustBePositive


mustNotBeNegative :: (Num a, Ord a, RenderMessage (HandlerSite m) YHCommonMessage, Monad m)
                  => Field m a
                  -> Field m a
mustNotBeNegative = checkBool (> 0) MsgFormMsgMustBePositive


weekDaySelectField :: (RenderMessage site FormMessage, RenderMessage site YHCommonMessage)
                   => Field (HandlerT site IO) Int
-- {{{1
weekDaySelectField =
  selectFieldList
    [ (MsgMonday, 1)
    , (MsgTuesday, 2)
    , (MsgWednesday, 3)
    , (MsgThursday, 4)
    , (MsgFriday, 5)
    , (MsgSaturday, 6)
    , (MsgSunday, 7)
    ]
-- }}}1


weekDayMultiSelectField :: (RenderMessage site FormMessage, RenderMessage site YHCommonMessage)
                   => Field (HandlerT site IO) [Int]
-- {{{1
weekDayMultiSelectField =
  multiSelectFieldList
    [ (MsgMonday, 1)
    , (MsgTuesday, 2)
    , (MsgWednesday, 3)
    , (MsgThursday, 4)
    , (MsgFriday, 5)
    , (MsgSaturday, 6)
    , (MsgSunday, 7)
    ]
-- }}}1


-- | 这段代码从 yesod 中截取出来．
-- 其结果是一个可以因 CSRF 而设的token，用于手写 form 时，插入到form内部去
formTokenWidget :: (MonadBaseControl IO m, MonadThrow m, MonadIO m)
                => WidgetT site m ()
-- {{{1
formTokenWidget = do
  req <- handlerToWidget getRequest
  let tokenKey = asText "_token"

  toWidget $
    case reqToken req of
      Nothing -> mempty
      Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
-- }}}1


-- vim: set foldmethod=marker:
