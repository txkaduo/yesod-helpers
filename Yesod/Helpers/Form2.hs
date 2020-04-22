{-|
 Form functions that report errors about each form field.
 Most code are copied and modified from yesod-form.
-}
module Yesod.Helpers.Form2
    ( FieldErrors, oneFieldError, overallFieldError, nullFieldErrors, fieldErrorsToList
    , EMForm, SEMForm
    , runEMFormPost
    , runEMFormPostNoToken
    , runEMFormGet
    , generateEMFormPost
    , generateEMFormGet'
    , generateEMFormGet
    , emreq, emopt, emstatic, emstaticW
    , semreq, semreq2, semopt, semopt2, semreqOpt, semstatic, semstatic', semstaticW
    , semview, semviewW
    , addEMFieldError
    , overallFieldName
    , addEMOverallError, addEMOverallError', addEMOverallErrorOfResult
    , renderBootstrapES
    , renderBootstrapES'
    , renderBootstrap3ES
    , renderBootstrap3ES'
    , renderBootstrap4ES
    , renderBootstrap4ES'
    , jsendFormData
    , optTimeRangeEndpointField
    , reqTimeRangeEndpointField
    ) where

-- {{{1 imports
import ClassyPrelude.Yesod
import qualified Data.Text.Encoding         as TE
import qualified Control.Monad.Trans.State.Strict as SS

import Control.Monad.Trans.RWS              (RWST, tell, evalRWST)
import Control.Monad.Trans.Writer           (runWriterT, WriterT(..))
import qualified Control.Monad.Trans.Writer as W
import Data.Byteable                        (constEqBytes)
import Data.Time
import Network.Wai                          (requestMethod)
import Text.Blaze                           (Markup)
import Text.Blaze.Html.Renderer.Text        (renderHtml)
import Data.Aeson.Types                     (Pair)
import qualified Yesod.Helpers.Bootstrap4   as BS4

import Yesod.Helpers.JSend

import           Yesod.Form.Jquery
#if MIN_VERSION_yesod_form(1, 3, 8)
import Yesod.Form.Bootstrap3                ( renderBootstrap3
                                            , BootstrapFormLayout(BootstrapBasicForm)
                                            )
#endif

import Yesod.Compat
-- }}}1


newtype WrappedFieldSettings master = WrappedFieldSettings
                                        { unWrappedFieldSettings :: FieldSettings master }

-- | Use 'fsId' and 'fsName' to compare/sort.
getFieldSettingsCoreFields :: FieldSettings master -> (Maybe Text, Maybe Text)
getFieldSettingsCoreFields = fsName &&& fsId

instance Eq (WrappedFieldSettings master) where
    (==) (WrappedFieldSettings x) (WrappedFieldSettings y) =
        getFieldSettingsCoreFields x == getFieldSettingsCoreFields y

instance Ord (WrappedFieldSettings master) where
    compare (WrappedFieldSettings x) (WrappedFieldSettings y) =
        compare (getFieldSettingsCoreFields x) (getFieldSettingsCoreFields y)

instance Hashable (WrappedFieldSettings master) where
    hashWithSalt salt (WrappedFieldSettings x) =
        hashWithSalt salt (getFieldSettingsCoreFields x)
    hash (WrappedFieldSettings x) = hash (getFieldSettingsCoreFields x)


newtype FieldErrors master = FieldErrors
                                { unFieldErrors :: HashMap (Text, WrappedFieldSettings master) (Set Text) }

oneFieldError :: Text
                -> FieldSettings master
                -> Text
                -> FieldErrors master
oneFieldError name fs msg = FieldErrors $ singletonMap
                                            (name, WrappedFieldSettings fs)
                                            (singletonSet msg)

overallFieldName :: Text
overallFieldName = "__all__"

overallFieldError :: Text -> FieldErrors master
overallFieldError msg = oneFieldError overallFieldName (fieldSettingsLabel ("" :: Text)) msg

nullFieldErrors :: FieldErrors master -> Bool
nullFieldErrors = null . unFieldErrors

fieldErrorsToList :: FieldErrors master -> [((Text, FieldSettings master), [Text])]
fieldErrorsToList = map (second unWrappedFieldSettings *** toList) .
                    mapToList .
                    unFieldErrors

fieldErrorsToJSON :: (SomeMessage master ->Text)
                    -> FieldErrors master
                    -> Value
fieldErrorsToJSON render_msg = object . map json_it . mapToList . unFieldErrors
    where
        json_it ((name, fs), v) = (name, object [ "fs" .= json_fs fs, "errs" .= toJSON (toList v) ])
        json_fs (WrappedFieldSettings x) = object
                    [ "id" .= fsId x
                    , "label" .= render_msg (fsLabel x)
                    , "tooltip" .= fmap render_msg (fsTooltip x)
                    ]

instance Monoid (FieldErrors master) where
    mempty  = FieldErrors mempty
    mappend (FieldErrors x1) (FieldErrors x2) = FieldErrors $ unionWith mappend x1 x2

instance Semigroup (FieldErrors master) where
    (<>) = mappend


type EMForm m = WriterT
                  (FieldErrors (HandlerSite m))
                  (RWST (Maybe (Env, FileEnv), HandlerSite m, [Lang]) Enctype Ints m)


-- | Usage: With the following helpers (smreq, smopt), all FieldView's are remembered.
-- So usually we don't need to name all FieldView's one by one,
-- which simplify code a little.
type SEMForm m a = SS.StateT [FieldView (HandlerSite m)] (EMForm m) a


-- | This function is used to both initially render a form and to later extract
-- results from it. Note that, due to CSRF protection and a few other issues,
-- forms submitted via GET and POST are slightly different. As such, be sure to
-- call the relevant function based on how the form will be submitted, /not/
-- the current request method.
--
-- For example, a common case is displaying a form on a GET request and having
-- the form submit to a POST page. In such a case, both the GET and POST
-- handlers should use 'runFormPost'.
runEMFormPost :: (RenderMessage (HandlerSite m) FormMessage, MonadResource m, MonadHandler m)
              => (Html -> EMForm m (FormResult a, xml))
              -> m (((FormResult a, xml), Enctype), FieldErrors (HandlerSite m))
-- {{{1
runEMFormPost form = do
    env <- postEnv
    postHelper form env
-- }}}1

runEMFormPostNoToken :: MonadHandler m
                     => (Html -> EMForm m a)
                     -> m ((a, Enctype), FieldErrors (HandlerSite m))
-- {{{1
runEMFormPostNoToken form = do
    langs <- languages
    m <- getYesod
    env <- postEnv
    runEMFormGeneric (form mempty) m langs env
-- }}}1

runEMFormGet :: MonadHandler m
             => (Html -> EMForm m a)
             -> m ((a, Enctype), FieldErrors (HandlerSite m))
-- {{{1
runEMFormGet form = do
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup getKey gets of
                Nothing -> Nothing
                Just _ -> Just (unionsWith (++) $ map (\(x, y) -> singletonMap x [y]) gets, mempty)
    getHelper form env
-- }}}1

-- | Similar to 'runFormPost', except it always ignores the currently available
-- environment. This is necessary in cases like a wizard UI, where a single
-- page will both receive and incoming form and produce a new, blank form. For
-- general usage, you can stick with @runFormPost@.
generateEMFormPost
    :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
    => (Html -> EMForm m (FormResult a, xml))
    -> m ((xml, Enctype), FieldErrors (HandlerSite m))
generateEMFormPost form = first (first snd) `liftM` postHelper form Nothing

generateEMFormGet' :: (MonadHandler m)
                   => (Html -> EMForm m (FormResult a, xml))
                   -> m ((xml, Enctype), FieldErrors (HandlerSite m))
generateEMFormGet' form = first (first snd) `liftM` getHelper form Nothing

generateEMFormGet :: MonadHandler m
                => (Html -> EMForm m a)
                -> m (a, Enctype)
generateEMFormGet form = liftM fst $ getHelper form Nothing

runEMFormGeneric :: Monad m
               => EMForm m a
               -> HandlerSite m
               -> [Text]
               -> Maybe (Env, FileEnv)
               -> m ((a, Enctype), FieldErrors (HandlerSite m))
-- {{{1
runEMFormGeneric form site langs env = do
    ((res, err_fields), enctype) <- evalRWST (runWriterT form) (env, site, langs) (IntSingle 0)
    return ((res, enctype), err_fields)
-- }}}1

postEnv :: (MonadHandler m) => m (Maybe (Env, FileEnv))
-- {{{1
postEnv = do
    req <- getRequest
    if requestMethod (reqWaiRequest req) == "GET"
        then return Nothing
        else do
            (p, f) <- runRequestBody
            let p' = unionsWith (++) $ map (\(x, y) -> singletonMap x [y]) p
            return $ Just (p', unionsWith (++) $ map (\(k, v) -> singletonMap k [v]) f)
-- }}}1

postHelper  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
            => (Html -> EMForm m (FormResult a, xml))
            -> Maybe (Env, FileEnv)
            -> m (((FormResult a, xml), Enctype), FieldErrors (HandlerSite m))
-- {{{1
postHelper form env = do
    req <- getRequest
    let tokenKey =
#if MIN_VERSION_yesod_core(1, 4, 14)
                  defaultCsrfParamName
#else
                  asText "_token"
#endif
    let token =
            case reqToken req of
                Nothing -> mempty
                Just n -> [shamlet|<input type=hidden name=#{tokenKey} value=#{n}>|]
    m <- getYesod
    langs <- languages
    (((res, xml), enctype), err_fields) <- runEMFormGeneric (form token) m langs env
    (res', err_fields') <- do
            let (Just [t1]) === (Just t2) = TE.encodeUtf8 t1 `constEqBytes` TE.encodeUtf8 t2
                Nothing     === Nothing   = True   -- It's important to use constTimeEq
                _           === _         = False  -- in order to avoid timing attacks.
            case (res, env) of
                (_, Nothing) -> return (FormMissing, err_fields)
                (FormSuccess{}, Just (params, _))
                    | not (lookup tokenKey params === reqToken req) -> do
                        let err_msg = renderMessage m langs MsgCsrfWarning
                        return ( FormFailure [err_msg]
                                , err_fields `mappend` overallFieldError err_msg
                                )
                _ -> return (res, err_fields)

    return (((res', xml), enctype), err_fields')
-- }}}1


-- | Converts a form field into monadic form. This field requires a value
-- and will return 'FormFailure' if left empty.
emreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
     => Field m a           -- ^ form field
     -> FieldSettings site  -- ^ settings for this field
     -> Maybe a             -- ^ optional default value
     -> EMForm m (FormResult a, FieldView site)
emreq field fs mdef = mhelper field fs mdef
                        (\m l name -> do
                            let err_msg = renderMessage m l MsgValueRequired
                            W.tell $ oneFieldError name fs err_msg
                            return $ FormFailure [err_msg]
                        )
                        FormSuccess True

-- | Converts a form field into monadic form. This field is optional, i.e.
-- if filled in, it returns 'Just a', if left empty, it returns 'Nothing'.
-- Arguments are the same as for 'mreq' (apart from type of default value).
emopt :: (site ~ HandlerSite m, MonadHandler m)
     => Field m a
     -> FieldSettings site
     -> Maybe (Maybe a)
     -> EMForm m (FormResult (Maybe a), FieldView site)
emopt field fs mdef = mhelper field fs
                        (join mdef) (const $ const $ const $ return $ FormSuccess Nothing)
                        (FormSuccess . Just) False

emstatic :: (site ~ HandlerSite m, MonadHandler m)
    => FieldSettings site
    -> a
    -> Text
    -> EMForm m (FormResult a, FieldView site)
emstatic (FieldSettings {..}) v text = do
    theId <- lift $ lift $ maybe newIdent return fsId
    (_, site, langs) <- lift $ ask
    let mr2 = renderMessage site langs
    return (FormSuccess v, FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = toWidget
            [shamlet|<p id="#{theId}" *{fsAttrs} .form-control-static .form-control-plaintext>#{text}|]
        , fvErrors = Nothing
        , fvRequired = False
        })


emstaticW :: (site ~ HandlerSite m, MonadHandler m, ToWidget site w)
          => FieldSettings site
          -> a
          -> w
          -> EMForm m (FormResult a, FieldView site)
emstaticW (FieldSettings {..}) v msg = do
    theId <- lift $ lift $ maybe newIdent return fsId
    (_, site, langs) <- lift $ ask
    let mr2 = renderMessage site langs
    return (FormSuccess v, FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = toWidget
            [whamlet|<p id="#{theId}" *{fsAttrs} .form-control-static>^{msg}|]
        , fvErrors = Nothing
        , fvRequired = False
        })


addEMFieldError :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                => Text
                -> FieldSettings (HandlerSite m)
                -> msg
                -> EMForm m ()
addEMFieldError name fs msg = do
    mr <- lift getMessageRender
    W.tell $ oneFieldError name fs (mr msg)

addEMOverallError :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                    => msg
                    -> EMForm m ()
addEMOverallError msg = do
    mr <- lift getMessageRender
    W.tell $ overallFieldError (mr msg)


addEMOverallError' :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                   => msg
                   -> EMForm m (FormResult a)
addEMOverallError' msg = do
    mr <- lift getMessageRender
    let msg' = mr msg
    W.tell $ overallFieldError msg'
    return $ FormFailure [ msg' ]



addEMOverallErrorOfResult :: MonadHandler m => FormResult a -> EMForm m ()
addEMOverallErrorOfResult field_result =
  case field_result of
    FormFailure errs -> mapM_ addEMOverallError errs
    _                -> return ()


semreq :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
       => Field m a
       -> FieldSettings site
       -> Maybe a
       -> SEMForm m (FormResult a)
semreq field settings initv = fmap fst $ semreq2 field settings initv

semreq2 :: (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m)
        => Field m a
        -> FieldSettings site
        -> Maybe a
        -> SEMForm m (FormResult a, FieldView site)
semreq2 field settings initv = do
    (res, view) <- lift $ emreq field settings initv
    SS.modify ( view : )
    return (res, view)


semopt :: (HandlerSite m ~ site, MonadHandler m)
       => Field m a
       -> FieldSettings site
       -> Maybe (Maybe a)
       -> SEMForm m (FormResult (Maybe a))
semopt field settings initv = fmap fst $ semopt2 field settings initv

semopt2 :: (HandlerSite m ~ site, MonadHandler m)
        => Field m a
        -> FieldSettings site
        -> Maybe (Maybe a)
        -> SEMForm m (FormResult (Maybe a), FieldView site)
semopt2 field settings initv = do
    (res, view) <- lift $ emopt field settings initv
    SS.modify ( view : )
    return (res, view)

semstatic :: (HandlerSite m ~ site, MonadHandler m)
    => FieldSettings site
    -> a
    -> Text
    -> SEMForm m (FormResult a)
semstatic settings v text = do
  (res, view) <- lift $ emstatic settings v text
  SS.modify ( view : )
  return res

semstatic' :: (HandlerSite m ~ site, MonadHandler m)
    => Text
    -> FieldSettings site
    -> a
    -> SEMForm m (FormResult a)
semstatic' = flip $ flip . semstatic


semstaticW :: (site ~ HandlerSite m, MonadHandler m, ToWidget site w)
           => FieldSettings site
           -> a
           -> w
           -> SEMForm m (FormResult a)
semstaticW fs v w = do
  (res, view) <- lift $ emstaticW fs v w
  SS.modify ( view : )
  return res


semviewW :: (site ~ HandlerSite m, MonadHandler m, ToWidget site w)
         => w
         -> FieldSettings site
         -> SEMForm m ()
semviewW t fs = void $ semstaticW fs () t


semview :: (HandlerSite m ~ site, MonadHandler m)
        => Text -> FieldSettings site -> SEMForm m ()
semview t fs = void $ semstatic fs () t

-- | Use `semreq` internally, but make the signature like `semopt`.
-- Useful when whether some fields is required depends on other conditions.
semreqOpt :: (HandlerSite m ~ site, MonadHandler m, RenderMessage site FormMessage)
          => Field m a
          -> FieldSettings site
          -> Maybe (Maybe a)
          -> SEMForm m (FormResult (Maybe a))
semreqOpt field settings initv = do
  fmap (fmap Just) $ semreq field settings (join initv)


renderBootstrapES :: Monad m =>
                    Markup
                    -> FormResult a
                    -> SEMForm m (FormResult a, WidgetOf (HandlerSite m))
-- {{{1
renderBootstrapES extra result = do
    views <- liftM reverse $ SS.get
    let aform = formToAForm $ return (result, views)
    lift $ lift $
#if MIN_VERSION_yesod_form(1, 3, 8)
        -- renderBootstrap is deprecated, but the new recommended function
        -- is for bootstrap v3.
        -- We assume that bootstrap v3 is used here.
        renderBootstrap3 BootstrapBasicForm
#else
        renderBootstrap
#endif
            aform extra
-- }}}1


#if MIN_VERSION_yesod_form(1, 3, 8)
renderBootstrap3ES :: Monad m
                   => BootstrapFormLayout
                   -> Markup
                   -> FormResult a
                   -> SEMForm m (FormResult a, WidgetOf (HandlerSite m))
renderBootstrap3ES layout extra result = do
    views <- liftM reverse $ SS.get
    let aform = formToAForm $ return (result, views)
    lift $ lift $ renderBootstrap3 layout aform extra
#endif

renderBootstrap4ES :: (MonadHandler m, Yesod (HandlerSite m))
                   => BS4.BootstrapFormLayout
                   -> Markup
                   -> FormResult a
                   -> SEMForm m (FormResult a, WidgetOf (HandlerSite m))
renderBootstrap4ES layout extra result = do
    views <- liftM reverse $ SS.get
    let aform = formToAForm $ return (result, views)
    lift $ lift $ BS4.renderBootstrap4 layout aform extra


-- | combines renderBootstrapS and runSEMForm, smToForm
renderBootstrapES' :: Monad m =>
    Markup
    -> SEMForm m (FormResult a)
    -> EMForm m (FormResult a, WidgetOf (HandlerSite m))
renderBootstrapES' extra result = do
    runSEMForm $ result >>= renderBootstrapES extra


#if MIN_VERSION_yesod_form(1, 3, 8)
renderBootstrap3ES' :: Monad m
                    => BootstrapFormLayout
                    -> Markup
                    -> SEMForm m (FormResult a)
                    -> EMForm m (FormResult a, WidgetOf (HandlerSite m))
renderBootstrap3ES' layout extra result = do
  runSEMForm $ result >>= renderBootstrap3ES layout extra
#endif


renderBootstrap4ES' :: (MonadHandler m, Yesod (HandlerSite m))
                    => BS4.BootstrapFormLayout
                    -> Markup
                    -> SEMForm m (FormResult a)
                    -> EMForm m (FormResult a, WidgetOf (HandlerSite m))
renderBootstrap4ES' layout extra result = do
  runSEMForm $ result >>= renderBootstrap4ES layout extra


runSEMForm :: Monad m => SEMForm m a -> EMForm m a
runSEMForm = flip SS.evalStateT []


-- | our standard way to encode a form and its errors into a JSON value.
jsendFormData :: (SomeMessage master -> Text)
                -> Maybe Html
                    -- ^ html code of the form body
                    -- sometimes client don't need the html code (just the errors are needed)
                    -- to save bandwidth, html code is optional
                -> FieldErrors master
                -> [Pair]
                -> JSendMsg
-- {{{1
jsendFormData render_msg m_form_html field_errs extra_fields =
    if nullFieldErrors field_errs
        then JSendSuccess dat
        else JSendFail dat
    where
        dat = object $
                [ "form" .= object
                            (catMaybes $
                                [ fmap (("body" .=) . renderHtml) m_form_html
                                , Just $ "errors" .= fieldErrorsToJSON render_msg field_errs
                                ]
                                )
                ] ++ extra_fields
-- }}}1


mhelper :: (site ~ HandlerSite m, MonadHandler m)
        => Field m a
        -> FieldSettings site
        -> Maybe a
        -> (site -> [Text] -> Text -> EMForm m (FormResult b)) -- ^ on missing
        -> (a -> FormResult b) -- ^ on success
        -> Bool -- ^ is it required?
        -> EMForm m (FormResult b, FieldView site)
-- {{{1
mhelper Field {..} fs@(FieldSettings {..}) mdef onMissing onFound isReq = do
    lift $ tell fieldEnctype
    mp <- lift $ askParams
    name <- lift $ maybe newFormIdent return fsName
    theId <- lift $ lift $ maybe newIdent return fsId
    (_, site, langs) <- lift $ ask
    let mr2 = renderMessage site langs
    (res, val) <-
        case mp of
            Nothing -> return (FormMissing, maybe (Left "") Right mdef)
            Just p -> do
                mfs <- lift askFiles
                let mvals = fromMaybe [] $ lookup name p
                    files = fromMaybe [] $ mfs >>= lookup name
                emx <- lift $ lift $ fieldParse mvals files
                case emx of
                    Left (SomeMessage e) -> do
                        let err_msg = renderMessage site langs e
                        W.tell $ oneFieldError name fs err_msg
                        return $ (FormFailure [err_msg], maybe (Left "") Left (listToMaybe mvals))
                    Right mx ->
                        case mx of
                            Nothing -> do
                                r <- onMissing site langs name
                                return (r, Left "")
                            Just x -> return (onFound x, Right x)
    return (res, FieldView
        { fvLabel = toHtml $ mr2 fsLabel
        , fvTooltip = fmap toHtml $ fmap mr2 fsTooltip
        , fvId = theId
        , fvInput = fieldView theId name fsAttrs val isReq
        , fvErrors =
            case res of
                FormFailure [e] -> Just $ toHtml e
                _ -> Nothing
        , fvRequired = isReq
        })
-- }}}1

getKey :: Text
getKey = "_hasdata"

getHelper :: MonadHandler m
          => (Html -> EMForm m a)
          -> Maybe (Env, FileEnv)
          -> m ((a, Enctype), FieldErrors (HandlerSite m))
-- {{{1
getHelper form env = do
    let fragment = [shamlet|<input type=hidden name=#{getKey}>|]
    langs <- languages
    m <- getYesod
    runEMFormGeneric (form fragment) m langs env
-- }}}1


-- | 用一个 Day 和 TimeOfDay 的输入，合成一个时间范围的端点（开始或结束）
optTimeRangeEndpointField :: (RenderMessage site FormMessage, YesodJquery site)
                          => TimeZone
                          -> Bool
                          -> FieldSettings site
                          -> FieldSettings site
                          -> Maybe UTCTime
                          -> SEMForm (HandlerOf site) (FormResult (Maybe UTCTime))
-- {{{1
optTimeRangeEndpointField tz is_start day_fs tod_fs old = do
  day <- semopt (jqueryDayField def) day_fs
            (Just $ fmap (localDay . utcToLocalTime tz) $ old)

  tod <- semopt timeFieldTypeTime tod_fs
            (Just $ fmap (localTimeOfDay . utcToLocalTime tz) $ old)

  compose_utc_time_field_result day tod

  where
    compose_utc_time Nothing (Just _)   = Left $ asText "需先指定日期"
    compose_utc_time Nothing Nothing    = Right Nothing
    compose_utc_time (Just d) m_tod = Right $ Just $
      localTimeToUTC tz $
        LocalTime d $
          case m_tod of
            Nothing -> if is_start
                          then midnight
                          else TimeOfDay 23 59 59.9999999

            Just tod -> tod


    compose_utc_time_field_result (FormSuccess d) (FormSuccess tod)   = case compose_utc_time d tod of
                                                                          Left err ->
                                                                            lift (addEMOverallError err) >> return FormMissing
                                                                          Right x -> return $ FormSuccess x

    compose_utc_time_field_result _               _                   = return FormMissing -- 每个参数的错误已被单独收集，这里不用处理
-- }}}1


-- | 用一个 Day 和 TimeOfDay 的输入，合成一个时间范围的端点（开始或结束）
reqTimeRangeEndpointField :: (RenderMessage site FormMessage, YesodJquery site)
                          => TimeZone
                          -> Bool
                          -> FieldSettings site
                          -> FieldSettings site
                          -> Maybe UTCTime
                          -> SEMForm (HandlerOf site) (FormResult UTCTime)
-- {{{1
reqTimeRangeEndpointField tz is_start day_fs tod_fs old_time = do
  day <- semreq (jqueryDayField def) day_fs
            (fmap (localDay . utcToLocalTime tz) old_time)

  tod <- semopt timeFieldTypeTime tod_fs
            (Just $ fmap (localTimeOfDay . utcToLocalTime tz) old_time)

  return $ compose_utc_time <$> day <*> tod

  where
    compose_utc_time d (Just t) = localTimeToUTC tz $ LocalTime d t
    compose_utc_time d Nothing = localTimeToUTC tz $ LocalTime d $
                                  if is_start
                                     then midnight
                                     else TimeOfDay 23 59 59.9999999
-- }}}1


-- vim: set foldmethod=marker:
