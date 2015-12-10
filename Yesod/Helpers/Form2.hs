{-|
 Form functions that report errors about each form field.
 Most code are copied and modified from yesod-form.
-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Yesod.Helpers.Form2
    ( FieldErrors, oneFieldError, overallFieldError, nullFieldErrors, fieldErrorsToList
    , EMForm, SEMForm
    , runEMFormPost
    , runEMFormPostNoToken
    , runEMFormGet
    , generateEMFormPost
    , generateEMFormGet'
    , generateEMFormGet
    , emreq, emopt
    , semreq, semopt
    , addEMFieldError
    , addEMOverallError
    , renderBootstrapES
    , renderBootstrapES'
    , jsendFormData
    ) where

import Prelude
import Yesod
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified Data.HashMap.Strict        as HM
import qualified Data.Text.Encoding         as TE
import qualified Control.Monad.Trans.State.Strict as SS

import Data.Set                             (Set)
import Data.HashMap.Strict                  (HashMap)
import Data.Text                            (Text)
import Control.Monad.Trans.RWS              (RWST, ask, tell, evalRWST)
import Control.Monad.Trans.Writer           (runWriterT, WriterT(..))
import Control.Monad                        (join, liftM)
import Control.Arrow                        (first)
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid                          (Monoid(..))
#endif
import qualified Control.Monad.Trans.Writer as W
import Data.Byteable                        (constEqBytes)
import Network.Wai                          (requestMethod)
import Text.Blaze                           (Markup)
import Text.Blaze.Html.Renderer.Text        (renderHtml)
import Data.Aeson.Types                     (Pair)
import Data.Maybe

import Yesod.Helpers.JSend

-- import Yesod.Form
#if MIN_VERSION_yesod_form(1, 3, 8)
import Yesod.Form.Bootstrap3                ( renderBootstrap3
                                            , BootstrapFormLayout(BootstrapBasicForm)
                                            )
#endif


newtype FieldErrors = FieldErrors { unFieldErrors :: HashMap Text (Set Text) }

oneFieldError :: Text -> Text -> FieldErrors
oneFieldError name msg = FieldErrors $ HM.singleton name (Set.singleton msg)

overallFieldError :: Text -> FieldErrors
overallFieldError msg = FieldErrors $ HM.singleton "__all__" (Set.singleton msg)

nullFieldErrors :: FieldErrors -> Bool
nullFieldErrors = HM.null . unFieldErrors

fieldErrorsToList :: FieldErrors -> [(Text, [Text])]
fieldErrorsToList = HM.toList . HM.map Set.toList . unFieldErrors

instance Monoid FieldErrors where
    mempty  = FieldErrors mempty
    mappend (FieldErrors x1) (FieldErrors x2) = FieldErrors $ HM.unionWith mappend x1 x2

instance ToJSON FieldErrors where
    toJSON = Object . HM.map (toJSON . Set.toList) . unFieldErrors

type EMForm m a = WriterT
                        FieldErrors
                        (RWST (Maybe (Env, FileEnv), HandlerSite m, [Lang]) Enctype Ints m)
                        a

-- | the following long type synonym actually says:
-- type SEMForm site m a = SS.StateT [FieldView site] (EMForm m) a
-- but haskell does not allow partially applied synonym in the above line,
-- we have the expand the synonym manually.
--
-- Usage: With the following helpers (smreq, smopt), all FieldView's are remembered.
-- So usually we don't need to name all FieldView's one by one,
-- which simplify code a little.
type SEMForm m a = SS.StateT [FieldView (HandlerSite m)]
                    (WriterT
                        FieldErrors
                        (RWST (Maybe (Env, FileEnv), HandlerSite m, [Lang]) Enctype Ints m)
                    )
                    a


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
            -> m ((((FormResult a, xml), Enctype), Html), FieldErrors)
runEMFormPost form = do
    env <- postEnv
    postHelper form env

runEMFormPostNoToken :: MonadHandler m
                   => (Html -> EMForm m a)
                   -> m ((a, Enctype), FieldErrors)
runEMFormPostNoToken form = do
    langs <- languages
    m <- getYesod
    env <- postEnv
    runEMFormGeneric (form mempty) m langs env

runEMFormGet :: MonadHandler m
           => (Html -> EMForm m a)
           -> m ((a, Enctype), FieldErrors)
runEMFormGet form = do
    gets <- liftM reqGetParams getRequest
    let env =
            case lookup getKey gets of
                Nothing -> Nothing
                Just _ -> Just (Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) gets, Map.empty)
    getHelper form env

-- | Similar to 'runFormPost', except it always ignores the currently available
-- environment. This is necessary in cases like a wizard UI, where a single
-- page will both receive and incoming form and produce a new, blank form. For
-- general usage, you can stick with @runFormPost@.
generateEMFormPost
    :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
    => (Html -> EMForm m (FormResult a, xml))
    -> m (((xml, Enctype), Html), FieldErrors)
generateEMFormPost form = first (first $ first snd) `liftM` postHelper form Nothing

generateEMFormGet'
    :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
    => (Html -> EMForm m (FormResult a, xml))
    -> m ((xml, Enctype), FieldErrors)
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
               -> m ((a, Enctype), FieldErrors)
runEMFormGeneric form site langs env = do
    ((res, err_fields), enctype) <- evalRWST (runWriterT form) (env, site, langs) (IntSingle 0)
    return ((res, enctype), err_fields)

postEnv :: (MonadHandler m, MonadResource m)
        => m (Maybe (Env, FileEnv))
postEnv = do
    req <- getRequest
    if requestMethod (reqWaiRequest req) == "GET"
        then return Nothing
        else do
            (p, f) <- runRequestBody
            let p' = Map.unionsWith (++) $ map (\(x, y) -> Map.singleton x [y]) p
            return $ Just (p', Map.unionsWith (++) $ map (\(k, v) -> Map.singleton k [v]) f)

postHelper  :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
            => (Html -> EMForm m (FormResult a, xml))
            -> Maybe (Env, FileEnv)
            -> m ((((FormResult a, xml), Enctype), Html), FieldErrors)
postHelper form env = do
    req <- getRequest
    let tokenKey = "_token"
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
                    | not (Map.lookup tokenKey params === reqToken req) -> do
                        let err_msg = renderMessage m langs MsgCsrfWarning
                        return ( FormFailure [err_msg]
                                , err_fields `mappend` overallFieldError err_msg
                                )
                _ -> return (res, err_fields)

    return ((((res', xml), enctype), token), err_fields')


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
                            W.tell $ oneFieldError name err_msg
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

addEMFieldError :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                => Text
                -> msg
                -> EMForm m ()
addEMFieldError name msg = do
    mr <- lift getMessageRender
    W.tell $ oneFieldError name (mr msg)

addEMOverallError :: (MonadHandler m, RenderMessage (HandlerSite m) msg)
                    => msg
                    -> EMForm m ()
addEMOverallError msg = do
    mr <- lift getMessageRender
    W.tell $ overallFieldError (mr msg)

semreq ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    Field m a
    -> FieldSettings site
    -> Maybe a
    -> SEMForm m (FormResult a)
semreq field settings initv = do
    (res, view) <- lift $ emreq field settings initv
    SS.modify ( view : )
    return res

semopt ::
    (RenderMessage site FormMessage, HandlerSite m ~ site, MonadHandler m) =>
    Field m a
    -> FieldSettings site
    -> Maybe (Maybe a)
    -> SEMForm m (FormResult (Maybe a))
semopt field settings initv = do
    (res, view) <- lift $ emopt field settings initv
    SS.modify ( view : )
    return res

renderBootstrapES :: Monad m =>
                    Markup
                    -> FormResult a
                    -> SEMForm m (FormResult a, WidgetT (HandlerSite m) IO ())
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

-- | combines renderBootstrapS and runSEMForm, smToForm
renderBootstrapES' :: Monad m =>
    Markup
    -> SEMForm m (FormResult a)
    -> EMForm m (FormResult a, WidgetT (HandlerSite m) IO ())
renderBootstrapES' extra result = do
    runSEMForm $ result >>= renderBootstrapES extra

runSEMForm :: Monad m => SEMForm m a -> EMForm m a
runSEMForm = flip SS.evalStateT []


-- | our standard way to encode a form and its errors into a JSON value.
jsendFormData :: Maybe Html
                    -- ^ html code of the form body
                    -- sometimes client don't need the html code (just the errors are needed)
                    -- to save bandwidth, html code is optional
                -> FieldErrors
                -> [Pair]
                -> JSendMsg
jsendFormData m_form_html field_errs extra_fields =
    if nullFieldErrors field_errs
        then JSendSuccess dat
        else JSendFail dat
    where
        dat = object $
                [ "form" .= object
                            (catMaybes $
                                [ fmap (("body" .=) . renderHtml) m_form_html
                                , Just $ "errors" .= field_errs
                                ]
                                )
                ] ++ extra_fields

mhelper :: (site ~ HandlerSite m, MonadHandler m)
        => Field m a
        -> FieldSettings site
        -> Maybe a
        -> (site -> [Text] -> Text -> EMForm m (FormResult b)) -- ^ on missing
        -> (a -> FormResult b) -- ^ on success
        -> Bool -- ^ is it required?
        -> EMForm m (FormResult b, FieldView site)

mhelper Field {..} FieldSettings {..} mdef onMissing onFound isReq = do
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
                let mvals = fromMaybe [] $ Map.lookup name p
                    files = fromMaybe [] $ mfs >>= Map.lookup name
                emx <- lift $ lift $ fieldParse mvals files
                case emx of
                    Left (SomeMessage e) -> do
                        let err_msg = renderMessage site langs e
                        W.tell $ oneFieldError name err_msg
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

getKey :: Text
getKey = "_hasdata"

getHelper :: MonadHandler m
          => (Html -> EMForm m a)
          -> Maybe (Env, FileEnv)
          -> m ((a, Enctype), FieldErrors)
getHelper form env = do
    let fragment = [shamlet|<input type=hidden name=#{getKey}>|]
    langs <- languages
    m <- getYesod
    runEMFormGeneric (form fragment) m langs env
