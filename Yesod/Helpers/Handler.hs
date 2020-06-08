{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Handler where

-- {{{1 imports
import ClassyPrelude.Yesod hiding (runFakeHandler, requestHeaders)
import Yesod.Core.Types
import qualified Control.Monad.Trans.Reader as R
import qualified Data.ByteString.Char8      as B8
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as TE
import qualified Data.Vault.Lazy            as V
#if MIN_VERSION_yesod_core(1,4,0)
import Yesod.Core.Unsafe                    (runFakeHandler)
#else
import Yesod.Core                           (runFakeHandler)
#endif
import qualified Data.Map.Strict            as Map
import Control.Monad.Trans.Maybe

import Network.Wai                          (requestHeaders, rawQueryString, requestMethod, vault)
import Text.Blaze                           (Markup)
import Data.List                            (findIndex)
import Data.Aeson.Types                     (Pair)

import Yesod.Helpers.Form
import Yesod.Helpers.Form2
import Yesod.Helpers.Form                   (jsonOrHtmlOutputForm')
import Yesod.Helpers.Utils                  (nullToNothing, encodeUtf8Rfc5987)
import Yesod.Helpers.Pager
import Yesod.Helpers.ParamNames

import Yesod.Helpers.JSend                  (provideRepJsendAndJsonp, JSendMsg(JSendSuccess))

#if MIN_VERSION_yesod_core(1, 6, 0)
import Yesod.Core.Types                     (HandlerFor(..))
#endif

import Yesod.Compat
-- }}}1


-- | Tell browser never cache this response
-- See: http://stackoverflow.com/questions/49547/how-to-control-web-page-caching-across-all-browsers/2068407
neverCache :: MonadHandler m => m ()
neverCache = do
  addHeader "Cache-Control" "no-cache, no-store, must-revalidate"
  addHeader "Pragma" "no-cache"
  addHeader "Expires" "0"


setLastModified :: UTCTime -> HandlerOf site ()
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


getCurrentRoute' :: MonadHandler m => m (Route (HandlerSite m))
getCurrentRoute' = getCurrentRoute >>= maybe (fail "getCurrentRoute failed") return


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
type MkForm site a = Markup -> MForm (HandlerOf site) (FormResult a, WidgetOf site)

type MkEMForm site a = Markup -> EMForm (HandlerOf site) (FormResult a, WidgetOf site)

type FormHandlerOf site a = R.ReaderT (WidgetOf site, Enctype) (HandlerOf site) a

type GenFormData site = (WidgetOf site, Enctype)
type EFormHandlerOf site a = R.ReaderT (GenFormData site, FieldErrors site) (HandlerOf site) a


handleGetPostEMFormTc :: (Yesod site, RenderMessage site FormMessage)
                      => MkEMForm site a
                      -> (EFormHandlerOf site Html)
                      -> (a -> HandlerOf site TypedContent)
                      -> HandlerOf site TypedContent
handleGetPostEMFormTc form show_form handle_form_data = do
  handleGetPostEMForm form show_form' handle_form_data
  where show_form' = jsonOrHtmlOutputFormEX [] show_form


handleGetPost :: MonadHandler m => m a -> m a -> m a
handleGetPost get_func post_func = do
  req_method <- requestMethod <$> waiRequest
  case req_method of
    "GET" -> get_func
    "POST" -> post_func
    _ -> sendResponseStatus methodNotAllowed405 (asText "Method Not Allowed!")


handleGetPostEMForm :: (RenderMessage site FormMessage)
                    => MkEMForm site a
                    -> EFormHandlerOf site c
                    -> (a -> HandlerOf site c)
                    -> HandlerOf site c
-- {{{1
handleGetPostEMForm form show_form handle_form_data = do
  handleGetPost on_get on_post
  where
    on_get = generateEMFormPost form >>= runReaderT show_form

    on_post = do
      (((result, formWidget), formEnctype), form_errs) <- runEMFormPost form
      case result of
          FormMissing     -> flip runReaderT ((formWidget, formEnctype), form_errs) $ show_form
          FormFailure _   -> flip runReaderT ((formWidget, formEnctype), form_errs) $ show_form
          FormSuccess form_data -> handle_form_data form_data
-- }}}1


handleGetPostEMFormNoTokenTc :: (Yesod site, RenderMessage site FormMessage)
                             => MkEMForm site a
                             -> EFormHandlerOf site Html
                             -> (a -> HandlerOf site TypedContent)
                             -> HandlerOf site TypedContent
handleGetPostEMFormNoTokenTc form show_form handle_form_data = do
  handleGetPostEMFormNoToken form show_form' handle_form_data
  where show_form' = jsonOrHtmlOutputFormEX [] show_form


handleGetPostEMFormNoToken :: (RenderMessage site FormMessage)
                           => MkEMForm site a
                           -> EFormHandlerOf site c
                           -> (a -> HandlerOf site c)
                           -> HandlerOf site c
-- {{{1
handleGetPostEMFormNoToken form show_form handle_form_data = do
  handleGetPost on_get on_post
  where
    on_get =
      generateEMFormPost form >>= runReaderT show_form

    on_post = do
      (((result, formWidget), formEnctype), form_errs) <- runEMFormPostNoToken form
      case result of
          FormMissing     -> flip runReaderT ((formWidget, formEnctype), form_errs) $ show_form
          FormFailure _   -> flip runReaderT ((formWidget, formEnctype), form_errs) $ show_form
          FormSuccess form_data -> handle_form_data form_data
-- }}}1


-- | Handler a GET form
-- If form failed/missing, abort processing and output empty widget (except the form widget)
handleGetEMForm :: (ToTypedContent b, MonadHandler m)
                => (Html -> EMForm m (FormResult a, w))
                -- ^ 'MkEMForm a', the form function
                -> ((w, Enctype) -> FieldErrors (HandlerSite m) -> Maybe r -> m b)
                -- ^ a function to make some output.
                -- Maybe Text param: an error message
                -- first 'w': form widget
                -- 'r': extra content. If form failed/missing, this will be mempty, otherwise, it will be the output of function in next param.
                -> (a -> m r)
                -- ^ use the form result, to generate result data
                -> m b
-- {{{1
handleGetEMForm form show_widget f = do
  (((form_result, form_widget), form_enc), form_errs) <- runEMFormGet form
  let show_empty_form_page = do
        show_widget (form_widget, form_enc) form_errs Nothing

  case form_result of
    FormMissing -> show_empty_form_page >>= sendResponse
    FormFailure _errs -> do
      -- form errors already in 'form_errs'
      show_empty_form_page >>= sendResponse
    FormSuccess result -> do
      f result >>= show_widget (form_widget, form_enc) mempty . Just
-- }}}1


-- | Handler a GET form
-- Form result can be 'missing'
handleGetEMFormMaybe :: (ToTypedContent b, MonadHandler m)
                     => (Html -> EMForm m (FormResult a, w))
                     -- ^ 'MkEMForm a', the form function
                     -> ((w, Enctype) -> FieldErrors (HandlerSite m) -> Maybe r -> m b)
                     -- ^ a function to make some output.
                     -- first 'w': form widget
                     -- 'r': extra content. If form failed/missing, this will be mempty, otherwise, it will be the output of function in next param.
                     -- second 'w': extra content. If form failed/missing, this will be mempty, otherwise, it will be the output of function in next param.
                     -> (Maybe a -> m r)
                     -- ^ use the form result
                     -> m b
-- {{{1
handleGetEMFormMaybe form show_widget f = do
  (((form_result, form_widget), form_enc), form_errs) <- runEMFormGet form
  let show_empty_form_page = do
        show_widget (form_widget, form_enc) form_errs Nothing

  m_res <- case form_result of
    FormFailure _errs -> do
      -- form errors already in 'form_errs'
      show_empty_form_page >>= sendResponse
    FormMissing -> return Nothing
    FormSuccess result -> return $ Just result

  f m_res >>= show_widget (form_widget, form_enc) mempty . Just
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

generateFormPostHandler :: (RenderMessage site FormMessage, MonadHandler m, HandlerSite m ~ site)
                        => MkForm site r
                        -> FormHandlerOf site a
                        -> m a
generateFormPostHandler form_func fh = do
  liftMonadHandler $ generateFormPost form_func >>= R.runReaderT fh

generateEMFormPostHandler :: (RenderMessage site FormMessage, MonadHandler m, HandlerSite m ~ site)
                          => MkEMForm site r
                          -> EFormHandlerOf site a
                          -> m a
generateEMFormPostHandler form_func fh = do
  liftMonadHandler $ generateEMFormPost form_func >>= R.runReaderT fh

generateFormPostHandlerJH :: (Yesod site, RenderMessage site FormMessage, MonadHandler m, HandlerSite m ~ site)
                          => MkForm site r
                          -> ([Text] -> FormHandlerOf site Html)
                          -> m TypedContent
generateFormPostHandlerJH form_func show_page = do
    generateFormPostHandler form_func $ do
        jsonOrHtmlOutputFormX show_page []


generateEMFormPostHandlerJH :: (Yesod site, RenderMessage site FormMessage, MonadHandler m, HandlerSite m ~ site)
                            => MkEMForm site r
                            -> (EFormHandlerOf site Html)
                            -> m TypedContent
generateEMFormPostHandlerJH form_func show_page = do
    generateEMFormPostHandler form_func $ do
        jsonOrHtmlOutputFormEX [] show_page


runFormPostHandler :: (RenderMessage site FormMessage)
                   => MkForm site r
                   -> (FormResult r -> FormHandlerOf site a)
                   -> HandlerOf site a
runFormPostHandler form_func fh = do
    ((result, formWidget), formEnctype) <- runFormPost form_func
    R.runReaderT (fh result) (formWidget, formEnctype)


runFormPostHandlerJH :: (RenderMessage site FormMessage, RenderMessage site msg, Yesod site)
                     => MkForm site r                 -- ^ form function
                     -> ([Text] -> FormHandlerOf site Html)
                                                    -- ^ show html page function
                     -> (r -> HandlerOf site (Either [msg] TypedContent))
                                                    -- ^ function to handler success form result
                     -> HandlerOf site TypedContent
runFormPostHandlerJH form_func show_page h_ok = do
    runFormPostHandler form_func $ jsonOrHtmlOutputFormHandleResult show_page h_ok


jsonOrHtmlOutputFormX :: (Yesod site)
                      => ([Text] -> FormHandlerOf site Html)
                      -> [Text]
                      -> FormHandlerOf site TypedContent
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
jsonOrHtmlOutputFormEX :: (Yesod site)
                        => [Pair]
                        -> EFormHandlerOf site Html
                            -- ^ provide HTML content
                        -> EFormHandlerOf site TypedContent
jsonOrHtmlOutputFormEX extra_js_fields show_form = do
    r@((formWidget, _formEnctype), field_errs) <- R.ask
    lift $ do
        render_msg <- getMessageRender
        selectRep $ do
            provideRep $ R.runReaderT show_form r
            provideRepJsendAndJsonp $ do
                form_body <- widgetToBodyHtml formWidget
                return $ jsendFormData render_msg (Just form_body) field_errs extra_js_fields


jsonOrHtmlOutputFormHandleResult :: (Yesod site, RenderMessage site msg)
                                 => ([Text] -> FormHandlerOf site Html)
                                 -> (r -> HandlerOf site (Either [msg] TypedContent))
                                  -> FormResult r
                                  -> FormHandlerOf site TypedContent
jsonOrHtmlOutputFormHandleResult show_page f result = do
    let showf errs = jsonOrHtmlOutputFormX show_page errs
        showf' msgs = lift getMessageRender >>= showf . flip map msgs
    case result of
        FormMissing         -> showf ([] :: [Text])
        FormFailure errs    -> showf errs
        FormSuccess x       -> (lift $ f x) >>= either showf' return


-- | 如果是普通页面输出，则，调用 setMessageI 并重定向至指定的 route
-- 如果是 JSON 输出，则按操作成功的方式输出文字信息
jsonOrRedirect :: forall site message url.  (RenderMessage site message, RedirectUrl site url)
               => message
               -> url
               -> HandlerOf site TypedContent
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
            html :: HandlerOf site Html
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
withHasDataGetParam :: HandlerOf site a -> HandlerOf site a
withHasDataGetParam f = localYesodRequest add_has_data_req f
    where
        getKey = "_hasdata"     -- 这个值 Yesod 没有 export 出来

        add_has_data_req req =
                req { reqGetParams = add_has_data $ reqGetParams req }

        add_has_data ps = maybe ((getKey, "") : ps) (const ps) $
                                lookup getKey ps


-- | Run handler with 'get params' overridden
withGetParams :: [(Text, Text)] -> HandlerOf site a -> HandlerOf site a
withGetParams params f = localYesodRequest set_get_params f
  where set_get_params req = req { reqGetParams = params }


localYesodRequest :: (YesodRequest -> YesodRequest)
                  -> HandlerOf site a
                  -> HandlerOf site a
localYesodRequest change f =
#if MIN_VERSION_yesod_core(1, 6, 0)
  HandlerFor $ unHandlerFor f . modify_hd
#else
  HandlerT $ unHandlerT f . modify_hd
#endif
  where modify_hd hd = hd { handlerRequest = change $ handlerRequest hd }


localVault :: (V.Vault -> V.Vault)
           -> HandlerOf site a
           -> HandlerOf site a
localVault update_v = localYesodRequest f
  where f yr = yr { reqWaiRequest = g (reqWaiRequest yr) }
        g wai_req = wai_req { vault = update_v (vault wai_req) }


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


semHiddenRetainParams :: (RenderMessage (HandlerSite m) FormMessage, MonadHandler m)
                      => [Text]
                      -> SEMForm m ()
semHiddenRetainParams names = do
  retain_params <- retainHttpParams names
  forM_ retain_params $ \ (n, v) -> do
    void $ semopt hiddenField (nameToFs n) (Just $ Just v)


redirectToReturnUrl :: (MonadHandler m, RedirectUrl (HandlerSite m) url)
                    => url -> m a
redirectToReturnUrl = do
  (lk returnUrlParamName >>=) . flip maybe redirect . redirect
  where
      lk x = lookupPostParam x >>= maybe (lookupGetParam x) (return . Just)

withReturnUrl :: Route site -> WidgetOf site
withReturnUrl r = do
  ret <- getCurrentUrl
  [whamlet|@?{(r, [(returnUrlParamName, ret)])}|]

reqPathPieceParamPostGet :: (PathPiece a, MonadHandler m)
                         => Text
                         -> m a
reqPathPieceParamPostGet pname =
    optPathPieceParamPostGet pname >>= maybe (invalidArgs [pname]) return


optPathPieceParamPostGet :: (PathPiece a, MonadHandler m)
                         => Text
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


widgetToBodyHtml :: (Yesod site, MonadHandler m, HandlerSite m ~ site)
                 => WidgetOf site
                 -> m Html
widgetToBodyHtml widget = liftMonadHandler $ do
  widgetToPageContent widget >>= withUrlRenderer . pageBody


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


handlerSimplePagedPage :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage)
                       => Int
                       -> (Int -> Int -> m ((a, Value), Int))
                       -> (PagedResult -> Int -> a -> m Html)
                       -> m TypedContent
-- {{{
handlerSimplePagedPage npp get_data show_html = do
  let pager_settings = PagerSettings npp pn_param
  pn <- pagerGetCurrentPageNumGet pager_settings

  ((result_list, json), total_num) <- get_data npp pn
  paged <- runPager pager_settings pn total_num

  selectRep $ do
    provideRep $ do
      show_html paged total_num result_list

    provideRepJsendAndJsonp $ do
      return $ JSendSuccess json

  where pn_param = "p"
-- }}}


-- vim: set foldmethod=marker:
