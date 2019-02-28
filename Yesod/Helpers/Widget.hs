module Yesod.Helpers.Widget where

-- {{{1 imports
import ClassyPrelude.Yesod
import Text.Blaze (ToMarkup)
import Data.Time

import Yesod.Helpers.Message
import Yesod.Helpers.Form2
import Yesod.Helpers.Handler
import Yesod.Helpers.Utils
import Yesod.Helpers.FuzzyDay
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


formOverallErrorMessageWidget :: (RenderMessage site a, MonadIO m, MonadThrow m, MonadBaseControl IO m)
                              => Maybe a  -- ^ overall message
                              -> FieldErrors site
                              -> WidgetT site m ()
formOverallErrorMessageWidget = formOverallErrorMessageWidget' . fmap w_msg
  where w_msg t = [whamlet|
                    <div .form-group>
                      <span .err_msg>_{t}
                  |]


formOverallErrorMessageWidget' :: (ToWidget site a, MonadIO m, MonadThrow m, MonadBaseControl IO m)
                               => Maybe a -- ^ overall message
                               -> FieldErrors site
                               -> WidgetT site m ()
-- {{{1
formOverallErrorMessageWidget' m_err_msg form_errs =
  [whamlet|
    $if isJust m_err_msg || not (null overall_errors)
      <div .alert .alert-warning>
        $maybe err_msg <- m_err_msg
          ^{err_msg}

        $if not (null overall_errors)
          <ul>输入数据错误
            $forall (_, errs) <- overall_errors
              <li>#{intercalate ";" errs}

            $forall ((_name, fs), errs) <- other_errors
              <li> _{fsLabel fs}: #{intercalate ";" errs}
  |]
  where error_list = fieldErrorsToList form_errs
        (overall_errors, other_errors) = partition ((== overallFieldName) . fst . fst) error_list
-- }}}1


simpleShowFormWidget :: (MonadIO m, MonadThrow m, MonadBaseControl IO m, RenderMessage master msg)
                     => ByteString
                     -> Either (Route master) Text
                     -> GenFormData master
                     -> msg
                     -> WidgetT master m ()
-- {{{1
simpleShowFormWidget method action' (formWidget, formEnctype) submit_msg = do
  action <- case action' of
                  Right x -> return x
                  Left r -> do
                    url_render <- getUrlRender
                    return $ url_render r
  [whamlet|
    <form method=#{decodeUtf8 method} action="#{action}" enctype=#{formEnctype} .form-horizontal>
      ^{formWidget}
      <div .form-group>
        <div .submit-btn-container .col-xs-offset-3 .col-xs-9>
          <input .btn .btn-primary type=submit value=_{submit_msg}>
  |]
-- }}}1


-- | A simple widget to show a html POST form
simpleFormPageWidgetEither :: ( MonadIO m, MonadThrow m
                              , MonadBaseControl IO m
                              , RenderMessage master a
                              , RenderMessage master YHCommonMessage
                              )
                           => GenFormData master
                           -> Either (Route master) Text  -- ^ action url
                           -> Maybe a       -- ^ any global error message
                           -> FieldErrors master  -- ^ error messages for form fields
                           -> WidgetT master m ()
simpleFormPageWidgetEither = simpleFormPageWidgetEither' "POST"


-- | A simple widget to show a html form
simpleFormPageWidgetEither' :: ( MonadIO m, MonadThrow m
                              , MonadBaseControl IO m
                              , RenderMessage master a
                              , RenderMessage master YHCommonMessage
                              )
                            => ByteString   -- ^ method
                            -> GenFormData master
                            -> Either (Route master) Text  -- ^ action url
                            -> Maybe a       -- ^ any global error message
                            -> FieldErrors master  -- ^ error messages for form fields
                            -> WidgetT master m ()
-- {{{1
simpleFormPageWidgetEither' method (formWidget, formEnctype) action' m_err_msg form_errs = do
  [whamlet|
    ^{formOverallErrorMessageWidget m_err_msg form_errs}
    ^{simpleShowFormWidget method action' (formWidget, formEnctype) MsgSubmitForm}
  |]
-- }}}1


simpleFormPageWidget :: (MonadIO m, MonadThrow m
                        , MonadBaseControl IO m
                        , RenderMessage master a
                        , RenderMessage master YHCommonMessage
                        )
                     => GenFormData master
                     -> Route master  -- ^ action url
                     -> Maybe a       -- ^ any global error message
                     -> FieldErrors master  -- ^ error messages for form fields
                     -> WidgetT master m ()
simpleFormPageWidget (formWidget, formEnctype) action m_err_msg form_errs =
  simpleFormPageWidgetEither (formWidget, formEnctype) (Left action) m_err_msg form_errs


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


zhCnFormatUtcWidget :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
                    => String
                    -> UTCTime
                    -> WidgetT site m ()
zhCnFormatUtcWidget fmt time = do
  localTime <- liftBase $ utcToLocalZonedTime time
  [whamlet|
    <time datetime=#{formatTime defaultTimeLocale iso8601 time}>
      #{formatTime zhCnTimeLocale fmt localTime}
  |]
  where iso8601 = "%Y-%m-%dT%H:%M:%SZ"


zhCnFormatUtcWidgetDefault :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
                           => UTCTime
                           -> WidgetT site m ()
zhCnFormatUtcWidgetDefault = zhCnFormatUtcWidget "%Y-%m-%d %H:%M:%S"

fuzzyDayWidget :: (MonadIO m, MonadBaseControl IO m, MonadThrow m) => FuzzyDay -> WidgetT site m ()
fuzzyDayWidget fd = [whamlet|<time datetime=#{toPathPiece fd}>#{toPathPiece fd}|]

wshow :: (ToMarkup a, MonadWidget m) => a -> m ()
wshow = toWidget . toHtml


wMessage :: (RenderMessage site a, MonadIO m, MonadThrow m, MonadBaseControl IO m)
         => a
         -> WidgetT site m ()
wMessage x = [whamlet|_{x}|]


-- vim: set foldmethod=marker:
