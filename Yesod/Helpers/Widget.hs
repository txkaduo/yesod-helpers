module Yesod.Helpers.Widget where

-- {{{1 imports
import ClassyPrelude
import Yesod
import Text.Blaze (ToMarkup)
import Data.Time
import qualified Network.Wai as Wai
import System.IO.Unsafe (unsafePerformIO)
import Text.Blaze.Renderer.Utf8             (renderMarkup)
import Text.Cassius
import Text.Julius
import Text.Parsec.TX.Utils
import Text.Hamlet
import qualified Data.Text.Lazy as LT

import Yesod.Helpers.ParamNames
import Yesod.Helpers.Message
import Yesod.Helpers.Form2
import Yesod.Helpers.Handler
import Yesod.Helpers.Utils
import Yesod.Helpers.FuzzyDay
import Yesod.Helpers.Types

import qualified Crypto.Hash.MD5 as MD5
import qualified Data.Vault.Lazy as V

import Yesod.Compat
-- }}}1


-- | Merge 'class' attribute
mergeCssClassAttr :: Text -> Text -> Text
mergeCssClassAttr new old = unwords $ ordNub $ new' <> old'
  where new' = words new
        old' = words old

mergeCssClassInAttrs :: Text -> [(Text, Text)] -> [(Text, Text)]
mergeCssClassInAttrs cls_spec = insertWith mergeCssClassAttr "class" cls_spec


vaultKeyLoadedCss :: V.Key (IORef (Set ByteString))
vaultKeyLoadedCss = unsafePerformIO V.newKey
{-# NOINLINE vaultKeyLoadedCss #-}


vaultKeyLoadedJs :: V.Key (IORef (Set ByteString))
vaultKeyLoadedJs = unsafePerformIO V.newKey
{-# NOINLINE vaultKeyLoadedJs #-}


vaultKeyLoadedHtml :: V.Key (IORef (Set ByteString))
vaultKeyLoadedHtml = unsafePerformIO V.newKey
{-# NOINLINE vaultKeyLoadedHtml #-}


-- | Must install this middleware, if use 'loadOnceCssUrl', 'loadOnceJsUrl', etc
-- Put it in 'yesodMiddleware' of the Yesod instance.
yesodMiddlewareInsertVaultLoadedAsset :: HandlerOf site a -> HandlerOf site a
yesodMiddlewareInsertVaultLoadedAsset h = do
  loaded_css_set <- newIORef mempty
  loaded_js_set <- newIORef mempty
  loaded_html_set <- newIORef mempty
  localVault (V.insert vaultKeyLoadedCss loaded_css_set) $
    localVault (V.insert vaultKeyLoadedJs loaded_js_set) $
      localVault (V.insert vaultKeyLoadedHtml loaded_html_set) $
        h


-- | Load CSS content only once. 'yesodMiddlewareInsertVaultLoadedCssJs' must be installed.
-- Example:
--   loadOnceCssUrl $ $(luciusFie "xxx.lucius")
--   loadOnceCssUrl $ $(cassiusFie "xxx.lucius")
loadOnceCssUrl :: MonadWidget m => CssUrl (Route (HandlerSite m)) -> m ()
loadOnceCssUrl css_url = getUrlRenderParams >>= loadOnceCss . css_url


-- | Load CSS content only once. 'yesodMiddlewareInsertVaultLoadedCssJs' must be installed.
loadOnceCss :: MonadWidget m => Css -> m ()
-- {{{1
loadOnceCss css = do
  m_set_ref <- V.lookup vaultKeyLoadedCss . Wai.vault <$> waiRequest
  case m_set_ref of
    Nothing -> throwIO $ userError "yesodMiddlewareInsertVaultLoadedAsset not installed"
    Just s_ref -> do
      let css_txt = LT.strip $ renderCss css
      let md5 = MD5.hashlazy $ encodeUtf8 css_txt

      loaded <- member md5 <$> readIORef s_ref
      unless loaded $ do
        modifyIORef' s_ref (insertSet md5)
        toWidget css
-- }}}1


-- | Load JS content only once. 'yesodMiddlewareInsertVaultLoadedCssJs' must be installed.
-- Example:
--   loadOnceJsUrl $ $(juliusFie "xxx.julius")
loadOnceJsUrl :: (MonadWidget m, HandlerSite m ~ site) => JavascriptUrl (Route site) -> m ()
loadOnceJsUrl js_url = getUrlRenderParams >>= loadOnceJs . js_url


-- | Load JS content only once. 'yesodMiddlewareInsertVaultLoadedCssJs' must be installed.
loadOnceJs :: (MonadWidget m, HandlerSite m ~ site) => Javascript -> m ()
-- {{{1
loadOnceJs js_code = do
  m_set_ref <- V.lookup vaultKeyLoadedJs . Wai.vault <$> waiRequest
  case m_set_ref of
    Nothing -> throwIO $ userError "yesodMiddlewareInsertVaultLoadedAsset not installed"
    Just s_ref -> do
      let js_txt = LT.strip $ renderJavascript js_code
      let md5 = MD5.hashlazy $ encodeUtf8 js_txt

      loaded <- member md5 <$> readIORef s_ref
      unless loaded $ do
        modifyIORef' s_ref (insertSet md5)
        toWidget js_code
-- }}}1


loadOnceHtmlUrl :: (MonadWidget m, HandlerSite m ~ site) => HtmlUrl (Route site) -> m ()
loadOnceHtmlUrl html_url = getUrlRenderParams >>= loadOnceHtml . html_url


loadOnceHtmlUrlI38n :: (MonadWidget m, HandlerSite m ~ site, RenderMessage site msg)
                    => HtmlUrlI18n msg (Route site) -> m ()
loadOnceHtmlUrlI38n html_url_i18n = do
  render_url <- getUrlRenderParams
  render_msg <- getMessageRender
  loadOnceHtml $ html_url_i18n (toHtml . render_msg) render_url


loadOnceHtml :: (MonadWidget m, HandlerSite m ~ site) => Html -> m ()
-- {{{1
loadOnceHtml html = do
  m_set_ref <- V.lookup vaultKeyLoadedHtml . Wai.vault <$> waiRequest
  case m_set_ref of
    Nothing -> throwIO $ userError "yesodMiddlewareInsertVaultLoadedAsset not installed"
    Just s_ref -> do
      let html_bs = renderMarkup html
      let md5 = MD5.hashlazy html_bs
      loaded <- member md5 <$> readIORef s_ref
      unless loaded $ do
        modifyIORef' s_ref (insertSet md5)
        toWidget html
-- }}}1


-- | construct page title by merging pieces of texts, separated by MsgPageTitleSep
mergePageTitles :: ( RenderMessage (HandlerSite m) message
                    , RenderMessage (HandlerSite m) YHCommonMessage
                    , MonadWidget m
                    )
                => [message]
                -> m ()
-- {{{1
mergePageTitles parts = do
    mr <- getMessageRender
    mr2 <- getMessageRender
    setTitleI $ mconcat $ intersperse (mr MsgPageTitleSep) $ map mr2 parts
-- }}}1


formAllErrorMessagesWidget :: FieldErrors site -> WidgetOf site
-- {{{1
formAllErrorMessagesWidget form_errs =
  [whamlet|
    $if not (null overall_errors)
        $if not (null overall_errors)
          <div .alert .alert-warning>
            <ul>表单输入错误
              $forall (_, errs) <- overall_errors
                <li>#{intercalate ";" errs}

              $forall ((_name, fs), errs) <- other_errors
                <li> _{fsLabel fs}: #{intercalate ";" errs}
  |]
  where error_list = fieldErrorsToList form_errs
        (overall_errors, other_errors) = partition ((== overallFieldName) . fst . fst) error_list
-- }}}1


simpleShowFormWidget :: (RenderMessage master msg)
                     => ByteString
                     -> Either (Route master) Text
                     -> GenFormData master
                     -> msg
                     -> WidgetOf master
-- {{{1
simpleShowFormWidget method action' (formWidget, formEnctype) submit_msg = do
  action <- case action' of
                  Right x -> return x
                  Left r -> do
                    url_render <- getUrlRender
                    return $ url_render r
  [whamlet|
    <form method=#{decodeUtf8 method} action="#{action}" enctype=#{formEnctype} .form-horizontal .main-form>
      ^{formWidget}
      <div .form-group>
        <div .submit-btn-container .col-xs-offset-3 .col-xs-9 .offset-sm-3 .col-sm-9 >
          <input .btn .btn-primary type=submit value=_{submit_msg}>
  |]
-- }}}1


-- | A simple widget to show a html POST form
simpleFormPageWidgetEither :: (RenderMessage master YHCommonMessage)
                           => GenFormData master
                           -> Either (Route master) Text  -- ^ action url
                           -> FieldErrors master  -- ^ error messages for form fields
                           -> WidgetOf master
simpleFormPageWidgetEither = simpleFormPageWidgetEither' "POST"


-- | A simple widget to show a html form
simpleFormPageWidgetEither' :: (RenderMessage master YHCommonMessage)
                            => ByteString   -- ^ method
                            -> GenFormData master
                            -> Either (Route master) Text  -- ^ action url
                            -> FieldErrors master  -- ^ error messages for form fields
                            -> WidgetOf master
-- {{{1
simpleFormPageWidgetEither' method (formWidget, formEnctype) action' form_errs = do
  [whamlet|
    ^{formAllErrorMessagesWidget form_errs}
    ^{simpleShowFormWidget method action' (formWidget, formEnctype) MsgSubmitForm}
  |]
-- }}}1


simpleFormPageWidget :: (RenderMessage master YHCommonMessage)
                     => GenFormData master
                     -> Route master  -- ^ action url
                     -> FieldErrors master  -- ^ error messages for form fields
                     -> WidgetOf master
simpleFormPageWidget (formWidget, formEnctype) action form_errs =
  simpleFormPageWidgetEither (formWidget, formEnctype) (Left action) form_errs


-- | XXX: 因为现在的表单提交按键总是一句提交，又不能直接改
--        暂时用js后期去修改
jsSetSubmitButtonText :: Text -- ^ form selector
                      -> Text -- ^ text to set
                      -> JavascriptUrl url
-- {{{1
jsSetSubmitButtonText selector txt =
  [julius|
    $(function () {
      var submits = $(#{toJSON selector}).find(':submit');
      submits.filter('input').val(#{toJSON txt});
      submits.filter('button').text(#{toJSON txt});
    });
  |]
-- }}}1


zhCnLocalTimeWidget :: String -> LocalTime -> WidgetOf site
zhCnLocalTimeWidget fmt t =
  [whamlet|
    <time datetime=#{formatTime defaultTimeLocale iso8601 t}>
      #{formatTime zhCnTimeLocale fmt t}
  |]
  where iso8601 = "%Y-%m-%dT%H:%M:%S"


zhCnLocalTimeWidgetDefault :: LocalTime -> WidgetOf site
zhCnLocalTimeWidgetDefault = zhCnLocalTimeWidget "%Y-%m-%d %H:%M:%S"

zhCnLocalTimeWidgetNoSec :: LocalTime -> WidgetOf site
zhCnLocalTimeWidgetNoSec = zhCnLocalTimeWidget "%Y-%m-%d %H:%M"


znCnFormatZonedTimeWidget :: String -> ZonedTime -> WidgetOf site
znCnFormatZonedTimeWidget fmt zt = do
  [whamlet|
    <time datetime=#{formatTime defaultTimeLocale iso8601 zt}>
      #{formatTime zhCnTimeLocale fmt zt}
  |]
  where iso8601 = "%Y-%m-%dT%H:%M:%SZ"


znCnFormatZonedTimeWidgetDefault :: ZonedTime -> WidgetOf site
znCnFormatZonedTimeWidgetDefault = znCnFormatZonedTimeWidget "%Y-%m-%d %H:%M:%S"

znCnFormatZonedTimeWidgetNoSec :: ZonedTime -> WidgetOf site
znCnFormatZonedTimeWidgetNoSec = znCnFormatZonedTimeWidget "%Y-%m-%d %H:%M"


zhCnFormatUtcWidget :: String
                    -> UTCTime
                    -> WidgetOf site
zhCnFormatUtcWidget fmt time = do
  localTime <- liftIO $ utcToLocalZonedTime time
  [whamlet|
    <time datetime=#{formatTime defaultTimeLocale iso8601 time}>
      #{formatTime zhCnTimeLocale fmt localTime}
  |]
  where iso8601 = "%Y-%m-%dT%H:%M:%SZ"


zhCnFormatUtcWidgetDefault :: UTCTime
                           -> WidgetOf site
zhCnFormatUtcWidgetDefault = zhCnFormatUtcWidget "%Y-%m-%d %H:%M:%S"


zhCnFormatUtcWidgetNoSec :: UTCTime
                         -> WidgetOf site
zhCnFormatUtcWidgetNoSec = zhCnFormatUtcWidget "%Y-%m-%d %H:%M"


fuzzyDayWidget :: FuzzyDay -> WidgetOf site
fuzzyDayWidget fd = [whamlet|<time datetime=#{toPathPiece fd}>#{toPathPiece fd}|]


dayWidget :: (Day -> String)  -- ^ how to show day
          -> Day
          -> WidgetOf site
dayWidget show_day d = [whamlet|<time datetime=#{show d}>#{show_day d}|]


zhCnFuzzyDayWidget :: FuzzyDay -> WidgetOf site
zhCnFuzzyDayWidget fd =
  [whamlet|<time datetime=#{simpleEncode fd}>#{show_fd}|]
  where show_fd = case fd of
                    FuzzyDayY y -> tshow y <> "年"
                    FuzzyDayYM y m -> tshow y <> "年" <> tshow m <> "月"
                    FuzzyDayYMD y m d -> tshow y <> "年" <> tshow m <> "月" <> tshow d <> "日"


yearMonthWidget :: (YearMonth -> String) -> YearMonth -> WidgetOf site
yearMonthWidget show_ym ym = [whamlet|<time datetime=#{show ym}>#{show_ym ym}|]

zhCnYearMonthWidget :: YearMonth -> WidgetOf site
zhCnYearMonthWidget = yearMonthWidget show_ym
  where show_ym (YearMonth y m) = show y <> "年" <> show m <> "月"


wshow :: (ToMarkup a, MonadWidget m) => a -> m ()
wshow = toWidget . toHtml


wMessage :: (RenderMessage site a)
         => a
         -> WidgetOf site
wMessage x = [whamlet|_{x}|]


wHiddenReturnUrl :: WidgetOf site
wHiddenReturnUrl = do
  url <- getCurrentUrl
  [whamlet|<input type=hidden name=#{returnUrlParamName} value=#{url}>|]

-- vim: set foldmethod=marker:
