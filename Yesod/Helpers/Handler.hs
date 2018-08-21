{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Handler where

-- {{{1 imports
import ClassyPrelude.Yesod hiding (runFakeHandler, requestHeaders)
import Yesod.Core.Types                     (HandlerT(..), handlerRequest)
import qualified Control.Monad.Trans.Reader as R
import qualified Data.ByteString.Char8      as B8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
#if MIN_VERSION_yesod_core(1,4,0)
import Yesod.Core.Unsafe                    (runFakeHandler)
#else
import Yesod.Core                           (runFakeHandler)
#endif
import qualified Data.Map.Strict            as Map
import Control.Monad.Trans.Maybe

import Network.Wai                          (requestHeaders, rawQueryString, requestMethod)
import Text.Blaze                           (Markup)
import Data.List                            (findIndex)
import Data.Aeson.Types                     (Pair)

import Yesod.Helpers.Form2
import Yesod.Helpers.Form                   (jsonOrHtmlOutputForm')
import Yesod.Helpers.Utils                  (nullToNothing, encodeUtf8Rfc5987)
import Yesod.Helpers.Pager

import Yesod.Helpers.JSend                  (provideRepJsendAndJsonp, JSendMsg(JSendSuccess))
-- }}}1


-- | Tell browser never cache this response
-- See: http://stackoverflow.com/questions/49547/how-to-control-web-page-caching-across-all-browsers/2068407
neverCache :: MonadHandler m => m ()
neverCache = do
  addHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  addHeader "Pragma" "no-cache"
  addHeader "Expires" "0"


setLastModified :: UTCTime -> HandlerT site IO ()
setLastModified = addHeader "Last-Modified" . formatRFC1123


asAttachment :: MonadHandler m => Maybe Text -> m ()
asAttachment Nothing = addHeader "Content-Disposition" "attachment"
asAttachment (Just fn) = addHeader "Content-Disposition" $ "attachment; filename*=" <> decodeUtf8 (encodeUtf8Rfc5987 fn)


{-# DEPRECATED isXHR "use acceptsJson instead" #-}
isXHR :: (MonadHandler m) => m Bool
isXHR = do
    req <- waiRequest
    let req_with = lookup "X-Request-With" $ requestHeaders req
    return $ maybe False (== "XMLHTTPRequest") req_with


-- | Works like aceeptsJson, but for jsonp: check for javascript
-- Code stolen from: Yesod.Core.Json
acceptsJavascript :: MonadHandler m => m Bool
acceptsJavascript =  (maybe False ((== "application/javascript") . B8.takeWhile (/= ';'))
            .  listToMaybe
            .  reqAccept)
           `liftM` getRequest


-- | Check accepts for json or jsonp
acceptsJsonOrJsonp :: MonadHandler m => m Bool
acceptsJsonOrJsonp = liftM2 (||) acceptsJson acceptsJavascript


clientOriginalIp :: MonadHandler m => m (Maybe ByteString)
clientOriginalIp = do
  req <- waiRequest
  let headers = requestHeaders req

  let first_ip_in_header hn = do
        h <- lookup hn headers
        listToMaybe $ filter (not . null) . map (encodeUtf8 . T.strip) $ T.splitOn "," $ decodeUtf8 h

  -- Normally, only x-forwarded-for may contain multiple IPs.
  -- But 'http-reverse-proxy' may make x-real-ip to contain multiple ip (possibly a bug).
  -- So we always prepare for multiple IPs.
  let f1 = first_ip_in_header "X-Real-IP"
      f2 = first_ip_in_header "X-Forwarded-For"
      f3 = first_ip_in_header "Remote-Addr"

  return $ f1 <|> f2 <|> f3


getCurrentUrl :: MonadHandler m => m Text
getCurrentUrl = do
    req <- waiRequest
    current_route <- getCurrentRoute >>= maybe (error "getCurrentRoute failed") return
    url_render <- getUrlRender
    return $ url_render current_route <> TE.decodeUtf8 (rawQueryString req)

getCurrentUrlExtraQS :: MonadHandler m => Text -> m Text
getCurrentUrlExtraQS qs = do
    req <- waiRequest
    current_route <- getCurrentRoute >>= maybe (error "getCurrentRoute failed") return
    url_render <- getUrlRender
    return $ url_render current_route <> add_qs (TE.decodeUtf8 (rawQueryString req))
    where
        add_qs x = if T.null x
                    then qs
                    else x <> "&" <> qs

-- | form function type synonym
type MkForm site m a = Markup -> MForm (HandlerT site m) (FormResult a, WidgetT site IO ())

type MkEMForm site m a = Markup -> EMForm (HandlerT site m) (FormResult a, WidgetT site IO ())

type FormHandlerT site m a = R.ReaderT (WidgetT site IO (), Enctype) (HandlerT site m) a

type GenFormData site = (WidgetT site IO (), Enctype)
type EFormHandlerT site m a = R.ReaderT (GenFormData site, FieldErrors site) (HandlerT site m) a


handleGetPostEMFormTc :: (Yesod site, RenderMessage site FormMessage)
                      => MkEMForm site IO a
                      -> (Maybe e -> EFormHandlerT site IO Html)
                      -> ((Maybe e -> HandlerT site IO TypedContent) -> a -> HandlerT site IO TypedContent)
                      -> HandlerT site IO TypedContent
-- {{{1
handleGetPostEMFormTc form show_form handle_form_data = do
  handleGetPostEMForm form show_form' handle_form_data
  where
    show_form' = jsonOrHtmlOutputFormEX [] . show_form
-- }}}1


handleGetPost :: MonadHandler m => m a -> m a -> m a
handleGetPost get_func post_func = do
  req_method <- requestMethod <$> waiRequest
  case req_method of
    "GET" -> get_func
    "POST" -> post_func
    _ -> sendResponseStatus methodNotAllowed405 (asText "Method Not Allowed!")


handleGetPostEMForm :: (Yesod site, RenderMessage site FormMessage)
                    => MkEMForm site IO a
                    -> (Maybe e -> EFormHandlerT site IO c)
                    -> ((Maybe e -> HandlerT site IO c) -> a -> HandlerT site IO c)
                    -> HandlerT site IO c
-- {{{1
handleGetPostEMForm form show_form handle_form_data = do
  handleGetPost on_get on_post
  where
    on_get = generateEMFormPost form >>= runReaderT ((show_form Nothing))

    on_post = do
      (((result, formWidget), formEnctype), form_errs) <- runEMFormPost form
      let showf merr = do
              flip runReaderT ((formWidget, formEnctype), form_errs) $ do
                  show_form merr

      case result of
          FormMissing     -> showf Nothing
          FormFailure _   -> showf Nothing
          FormSuccess form_data -> handle_form_data showf form_data
-- }}}1


handleGetPostEMFormNoTokenTc :: (Yesod site, RenderMessage site FormMessage)
                             => MkEMForm site IO a
                             -> (Maybe Text -> EFormHandlerT site IO Html)
                             -> ((Maybe Text -> HandlerT site IO TypedContent) -> a -> HandlerT site IO TypedContent)
                             -> HandlerT site IO TypedContent
-- {{{1
handleGetPostEMFormNoTokenTc form show_form handle_form_data = do
  handleGetPostEMFormNoToken form show_form' handle_form_data
  where
    show_form' = jsonOrHtmlOutputFormEX [] . show_form
-- }}}1


handleGetPostEMFormNoToken :: (Yesod site, RenderMessage site FormMessage)
                           => MkEMForm site IO a
                           -> (Maybe Text -> EFormHandlerT site IO c)
                           -> ((Maybe Text -> HandlerT site IO c) -> a -> HandlerT site IO c)
                           -> HandlerT site IO c
-- {{{1
handleGetPostEMFormNoToken form show_form handle_form_data = do
  handleGetPost on_get on_post
  where
    on_get =
      generateEMFormPost form >>= runReaderT ((show_form Nothing))

    on_post = do
      (((result, formWidget), formEnctype), form_errs) <- runEMFormPostNoToken form
      let showf merr = do
              m_add_err <- liftM (fromMaybe mempty) $ forM merr $ \err -> do
                              return $ overallFieldError err
              flip runReaderT ((formWidget, formEnctype), form_errs <> m_add_err) $ do
                  show_form merr

      case result of
          FormMissing     -> showf Nothing
          FormFailure _   -> showf Nothing
          FormSuccess form_data -> handle_form_data showf form_data
-- }}}1


-- | Handler a GET form
-- If form failed/missing, abort processing and output empty widget (except the form widget)
handleGetEMForm :: (ToTypedContent b, MonadHandler m, w ~ WidgetT site IO ())
                => (Html -> EMForm m (FormResult a, w))
                -- ^ 'MkEMForm a', the form function
                -> ((w, Enctype) -> FieldErrors (HandlerSite m) -> Maybe Text -> Maybe w -> m b)
                -- ^ a function to make some output.
                -- Maybe Text param: an error message
                -- first 'w': form widget
                -- second 'w': extra content. If form failed/missing, this is mempty, otherwise, it is the output of function in next param.
                -> (a -> m w)
                -- ^ use the form result
                -> m b
-- {{{1
handleGetEMForm form show_widget f = do
  (((form_result, form_widget), form_enc), form_errs) <- runEMFormGet form
  let show_empty_form_page m_err = do
        show_widget (form_widget, form_enc) form_errs m_err Nothing

  case form_result of
    FormMissing -> show_empty_form_page Nothing >>= sendResponse
    FormFailure errs -> show_empty_form_page (Just $ intercalate "," errs) >>= sendResponse
    FormSuccess result -> do
      f result >>= show_widget (form_widget, form_enc) mempty Nothing . Just
-- }}}1


-- | Handler a GET form
-- Form result can be 'missing'
handleGetEMFormMaybe :: (ToTypedContent b, MonadHandler m, w ~ WidgetT site IO ())
                     => (Html -> EMForm m (FormResult a, w))
                     -- ^ 'MkEMForm a', the form function
                     -> ((w, Enctype) -> FieldErrors (HandlerSite m) -> Maybe Text -> Maybe w -> m b)
                     -- ^ a function to make some output.
                     -- Maybe Text param: an error message
                     -- first 'w': form widget
                     -- second 'w': extra content. If form failed/missing, this is mempty, otherwise, it is the output of function in next param.
                     -> (Maybe a -> m w)
                     -- ^ use the form result
                     -> m b
-- {{{1
handleGetEMFormMaybe form show_widget f = do
  (((form_result, form_widget), form_enc), form_errs) <- runEMFormGet form
  let show_empty_form_page m_err = do
        show_widget (form_widget, form_enc) form_errs m_err Nothing

  m_res <- case form_result of
    FormFailure errs -> show_empty_form_page (Just $ intercalate "," errs) >>= sendResponse
    FormMissing -> return Nothing
    FormSuccess result -> return $ Just result

  f m_res >>= show_widget (form_widget, form_enc) mempty Nothing . Just
-- }}}1

-- ^
-- Typical Usage:
--
-- xxxForm :: Form XXX
-- xxxForm extra = undefined
--
-- showXXXPage :: FormHandlerT App IO Html
-- showXXXPage = do
--     (formWidget, formEnctype) <- R.ask
--     lift $ defaultLayout $ do
--         $(widgetFile "xxx")
--
-- getXXXR :: Handler TypedContent
-- getXXXR = do
--     generateFormPostHandlerJH xxxForm showXXXPage
--
-- postAddJournalInfoR :: Handler TypedContent
-- postAddJournalInfoR = do
--     runFormPostHandlerJH xxxForm showXXXPage $
--         (\result -> undefined) :: (XXX -> Handler (Either [msg] TypedContent))

generateFormPostHandler ::
    ( Monad m, MonadThrow m, MonadBaseControl IO m, MonadIO m
    , RenderMessage site FormMessage
    ) =>
    MkForm site m r
    -> FormHandlerT site m a
    -> HandlerT site m a
generateFormPostHandler form_func fh = do
    generateFormPost form_func >>= R.runReaderT fh

generateEMFormPostHandler ::
    ( Monad m, MonadThrow m, MonadBaseControl IO m, MonadIO m
    , RenderMessage site FormMessage
    ) =>
    MkEMForm site m r
    -> EFormHandlerT site m a
    -> HandlerT site m a
generateEMFormPostHandler form_func fh = do
    generateEMFormPost form_func >>= R.runReaderT fh

generateFormPostHandlerJH ::
    ( Yesod site, RenderMessage site FormMessage
    , MonadIO m, MonadThrow m, MonadBaseControl IO m
    ) =>
    MkForm site m r
    -> ([Text] -> FormHandlerT site m Html)
    -> HandlerT site m TypedContent
generateFormPostHandlerJH form_func show_page = do
    generateFormPostHandler form_func $ do
        jsonOrHtmlOutputFormX show_page []

generateEMFormPostHandlerJH ::
    ( Yesod site, RenderMessage site FormMessage
    , MonadIO m, MonadThrow m, MonadBaseControl IO m
    ) =>
    MkEMForm site m r
    -> (EFormHandlerT site m Html)
    -> HandlerT site m TypedContent
generateEMFormPostHandlerJH form_func show_page = do
    generateEMFormPostHandler form_func $ do
        jsonOrHtmlOutputFormEX [] show_page

runFormPostHandler ::
    ( Monad m, MonadThrow m, MonadBaseControl IO m, MonadIO m
    , RenderMessage site FormMessage
    ) =>
    MkForm site m r
    -> (FormResult r -> FormHandlerT site m a)
    -> HandlerT site m a
runFormPostHandler form_func fh = do
    ((result, formWidget), formEnctype) <- runFormPost form_func
    R.runReaderT (fh result) (formWidget, formEnctype)

runFormPostHandlerJH ::
    (RenderMessage site FormMessage, RenderMessage site msg, Yesod site
    , MonadIO m, MonadBaseControl IO m, MonadThrow m
    ) =>
    MkForm site m r                 -- ^ form function
    -> ([Text] -> FormHandlerT site m Html)
                                    -- ^ show html page function
    -> (r -> HandlerT site m (Either [msg] TypedContent))
                                    -- ^ function to handler success form result
    -> HandlerT site m TypedContent
runFormPostHandlerJH form_func show_page h_ok = do
    runFormPostHandler form_func $
        jsonOrHtmlOutputFormHandleResult show_page h_ok


jsonOrHtmlOutputFormX :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
    ([Text] -> FormHandlerT site m Html)
    -> [Text]
    -> FormHandlerT site m TypedContent
jsonOrHtmlOutputFormX show_form errs = do
    (formWidget, formEnctype) <- R.ask
    let show_form' = R.runReaderT (show_form errs) (formWidget, formEnctype)
    lift $ jsonOrHtmlOutputForm' show_form' formWidget [ "form_errors" .= errs ]


-- | response with html content or json content
-- the JSON format is in "JSend" format, like this:
-- { "status": "success"
-- , "data": { "form":
--               { "body": /* form html code */
--               , "errors": { "field1", "error message1" }
--               }
--           }
-- }
jsonOrHtmlOutputFormEX :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m)
                        => [Pair]
                        -> EFormHandlerT site m Html
                            -- ^ provide HTML content
                        -> EFormHandlerT site m TypedContent
jsonOrHtmlOutputFormEX extra_js_fields show_form = do
    r@((formWidget, _formEnctype), field_errs) <- R.ask
    lift $ do
        render_msg <- getMessageRender
        selectRep $ do
            provideRep $ R.runReaderT show_form r
            provideRepJsendAndJsonp $ do
                form_body <- widgetToBodyHtml formWidget
                return $ jsendFormData render_msg (Just form_body) field_errs extra_js_fields

jsonOrHtmlOutputFormHandleResult ::
    (Yesod site, RenderMessage site msg
    , MonadIO m, MonadThrow m, MonadBaseControl IO m
    ) =>
    ([Text] -> FormHandlerT site m Html)
    -> (r -> HandlerT site m (Either [msg] TypedContent))
    -> FormResult r
    -> FormHandlerT site m TypedContent
jsonOrHtmlOutputFormHandleResult show_page f result = do
    let showf errs = jsonOrHtmlOutputFormX show_page errs
        showf' msgs = lift getMessageRender >>= showf . flip map msgs
    case result of
        FormMissing         -> showf ([] :: [Text])
        FormFailure errs    -> showf errs
        FormSuccess x       -> (lift $ f x) >>= either showf' return

-- | 如果是普通页面输出，则，调用 setMessageI 并重定向至指定的 route
-- 如果是 JSON 输出，则按操作成功的方式输出文字信息
jsonOrRedirect ::
    forall site message url.
    (RenderMessage site message, RedirectUrl site url) =>
    message -> url -> HandlerT site IO TypedContent
jsonOrRedirect msg route = do
    selectRep $ do
        provideRepJsendAndJsonp $ do
            msgRender <- getMessageRender
            let tmsg = msgRender msg
            turl <- toTextUrl route
            let dat = object $ [( "url" .= turl ), ( "msg" .= tmsg )]
            return $ JSendSuccess dat
        provideRep $ html
        where
            html :: HandlerT site IO Html
            html = do
                setMessageI $ msg
                redirect route

matchMimeType :: ContentType -> ContentType -> Bool
matchMimeType expected actual =
    (mainType == "*" || mainType0 == "*" || mainType == mainType0)
        && (subType == "*" || subType0 == "*" || subType == subType0)
    where
        (mainType, subType) = contentTypeTypes actual
        (mainType0, subType0) = contentTypeTypes expected


lookupReqAccept :: MonadHandler m => [(ContentType -> Bool, a)] -> m (Maybe a)
lookupReqAccept lst = do
    fmap reqAccept getRequest >>= return . tryAccepts
    where
        tryAccepts cts = listToMaybe $ map snd $
                            sortBy (comparing $ fst) $ catMaybes $
                                flip map lst $
                                    \(f, x) ->
                                        fmap (, x) $ findIndex f cts


-- | runFormGet needs a special parameter "_hasdata",
-- if it does not exist, runFormGet consider no form data input at all.
-- Use this function to workaround this.
-- XXX: getHelper auto add _hasdata to the form.
-- It'd better to add to params by yourself if submit by AJAX.
withHasDataGetParam :: HandlerT site m a -> HandlerT site m a
withHasDataGetParam f = withAlteredYesodRequest add_has_data_req f
    where
        getKey = "_hasdata"     -- 这个值 Yesod 没有 export 出来

        add_has_data_req req =
                req { reqGetParams = add_has_data $ reqGetParams req }

        add_has_data ps = maybe ((getKey, "") : ps) (const ps) $
                                lookup getKey ps


-- | modify YesodRequest before executing inner Handler code
withAlteredYesodRequest ::
    (YesodRequest -> YesodRequest)
    -> HandlerT site m a -> HandlerT site m a
withAlteredYesodRequest change f = HandlerT $ unHandlerT f . modify_hd
    where
        modify_hd hd = hd { handlerRequest = change $ handlerRequest hd }


-- | like FormResult, but used when you want to manually validate get/post params
data ParamResult a = ParamError [(Text, Text)]
                    | ParamSuccess a
                    deriving (Eq, Show)


instance Functor ParamResult where
    fmap _ (ParamError errs)    = ParamError errs
    fmap f (ParamSuccess x)     = ParamSuccess (f x)


instance Applicative ParamResult where
    pure = ParamSuccess

    (ParamError errs) <*> (ParamError errs2)    = ParamError (errs ++ errs2)
    (ParamError errs) <*> (ParamSuccess _)      = ParamError errs
    (ParamSuccess _) <*> (ParamError errs)      = ParamError errs
    (ParamSuccess f) <*> (ParamSuccess x)       = ParamSuccess $ f x


instance Monad ParamResult where
    return = pure

    (ParamError errs) >>= _ = ParamError errs
    (ParamSuccess x)  >>= f = f x

    fail msg = ParamError [("", T.pack msg)]


httpErrorRetryWithValidParams ::
    (IsString s, Semigroup s, ToTypedContent s, MonadHandler m) =>
    s -> m a
httpErrorRetryWithValidParams msg = do
    sendResponseStatus (mkStatus 449 "Retry With") $
        "Retry with valid parameters: " <> msg

httpErrorWhenParamError :: MonadHandler m => ParamResult a -> m a
httpErrorWhenParamError (ParamSuccess x)    = return x
httpErrorWhenParamError (ParamError errs)   =
    httpErrorRetryWithValidParams msg
    where
        msg = T.intercalate ", " $
                flip map errs $ \(param_name, err_msg) ->
                    if T.null err_msg
                        then param_name
                        else param_name <> "(" <> err_msg <> ")"


mkParamErrorNoMsg :: Text -> ParamResult a
mkParamErrorNoMsg p = ParamError [ (p, "") ]

reqGetParamE :: MonadHandler m => Text -> m (ParamResult Text)
reqGetParamE p = fmap (maybe (mkParamErrorNoMsg p) ParamSuccess) $ lookupGetParam p

reqGetParamE' :: MonadHandler m => Text -> m (ParamResult Text)
reqGetParamE' p = reqGetParamE p >>= return . nonEmptyParam p

reqPostParamE :: MonadHandler m => Text -> m (ParamResult Text)
reqPostParamE p = fmap (maybe (mkParamErrorNoMsg p) ParamSuccess) $ lookupPostParam p

reqPostParamE' :: MonadHandler m => Text -> m (ParamResult Text)
reqPostParamE' p = reqPostParamE p >>= return . nonEmptyParam p

nonEmptyParam :: Text -> ParamResult Text -> ParamResult Text
nonEmptyParam pn pr = do
    x <- pr
    if T.null x
        then ParamError [(pn, "must not be empty")]
        else ParamSuccess x

paramErrorFromEither :: Text -> Either Text a -> ParamResult a
paramErrorFromEither pn (Left err)  = ParamError [(pn, err)]
paramErrorFromEither _  (Right x)   = ParamSuccess x

validateParam :: Text -> (a -> Either Text b) -> ParamResult a -> ParamResult b
validateParam pn f pr = do
    pr >>= paramErrorFromEither pn . f


-- | 用于保持某些 get/post 变量，以便在 redirect 和 提交表单时可以传送給下一个服务器地址
retainHttpParams :: MonadHandler m => [Text] -> m [(Text, Text)]
retainHttpParams param_names = do
    fmap catMaybes $ mapM f param_names
    where
        f n = runMaybeT $ do
                val <- MaybeT (lookupPostParam n) <|> MaybeT (lookupGetParam n)
                return (n, val)


reqPathPieceParamPostGet :: (PathPiece a, MonadHandler m) =>
                            Text
                            -> m a
reqPathPieceParamPostGet pname =
    optPathPieceParamPostGet pname >>= maybe (invalidArgs [pname]) return

optPathPieceParamPostGet :: (PathPiece a, MonadHandler m) =>
                            Text
                            -> m (Maybe a)
optPathPieceParamPostGet pname =
    (fmap (join . fmap fromPathPiece) $ runMaybeT $
                (MaybeT $ join . fmap nullToNothing <$> lookupPostParam pname)
                <|> (MaybeT $ join . fmap nullToNothing <$> lookupGetParam pname))

getUrlRenderParamsIO :: Yesod site => site -> IO (Route site -> [(Text, Text)] -> Text)
getUrlRenderParamsIO foundation = do
        runFakeHandler Map.empty
                    (error "logger required in getUrlRenderParamsIO")
                    foundation getUrlRenderParams
        >>= either (error "getUrlRenderParams shoud never fail") return

getUrlRenderIO :: Yesod site => site -> IO (Route site -> Text)
getUrlRenderIO foundation = do
        runFakeHandler Map.empty
                    (error "logger required in getUrlRenderIO")
                    foundation getUrlRender
        >>= either (error "getUrlRender shoud never fail") return


widgetToBodyHtml :: (Yesod site, MonadIO m, MonadThrow m, MonadBaseControl IO m)
                => WidgetT site IO ()
                -> HandlerT site m Html
widgetToBodyHtml widget = liftHandlerT $ do
    widgetToPageContent widget
        >>= withUrlRenderer . pageBody


-- | 目前来说，大多数时候情况都不需要 i18n
-- 这个函数包装了 languages:
-- 仅在必要时才真正调用 languages，其它时候只返回固定的语言
defaultLangs :: MonadHandler m
                => Lang
                -> m [Lang]
defaultLangs def_lang = do
    b <- fromMaybe (0 :: Int) <$> optPathPieceParamPostGet "_i18n"
    if b > 0
        then languages
        else return [ def_lang ]

defaultZhCnLangs :: MonadHandler m
                => m [Lang]
defaultZhCnLangs = defaultLangs "zh-CN"


type PagedPage site a = Maybe Text
                     -> PagedResult
                     -> a
                     -> Int
                     -> EFormHandlerT site IO Html


handlerPagedPageWithForm' :: (NumPerPage so, Yesod site, RenderMessage site FormMessage)
                          => (Maybe so -> HandlerT site IO so)
                          -> MkEMForm site IO so
                          -> (so -> Int -> Int -> HandlerT site IO ((a, Value), Int))
                          -> PagedPage site a
                          -> HandlerT site IO TypedContent
-- {{{1
handlerPagedPageWithForm' mk_def_so form get_data show_html = do
  (((result, formWidget), formEnctype), form_errs) <- runEMFormGet form

  so <- mk_def_so $ case result of
                      FormSuccess x -> Just x
                      _ -> Nothing

  let npp = numPerPage so
  let pager_settings = PagerSettings npp pn_param
  pn <- pagerGetCurrentPageNumGet pager_settings

  ((result_list, json), total_num) <- get_data so npp pn
  paged <- runPager pager_settings pn total_num

  selectRep $ do
    provideRep $ do
      let showf merr results = do
            m_add_err <- liftM (fromMaybe mempty) $ forM merr $ \err -> do
                            return $ overallFieldError err
            flip runReaderT ((formWidget, formEnctype), form_errs <> m_add_err) $ do
              show_html merr paged results total_num

      showf Nothing result_list

    provideRepJsendAndJsonp $ do
      return $ JSendSuccess json

  where pn_param = "p"
-- }}}1



handlerPagedPageWithForm :: (NumPerPage so, Yesod site, RenderMessage site FormMessage)
                         => so
                         -> MkEMForm site IO so
                         -> (so -> Int -> Int -> HandlerT site IO ((a, Value), Int))
                         -> PagedPage site a
                         -> HandlerT site IO TypedContent
-- {{{1
handlerPagedPageWithForm def_so form get_data show_html = do
  handlerPagedPageWithForm' (return . fromMaybe def_so) form get_data show_html
-- }}}1



-- vim: set foldmethod=marker:
