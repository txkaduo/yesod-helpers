module Yesod.Helpers.Widget where

-- {{{1 imports
import ClassyPrelude.Yesod

import Yesod.Helpers.Message
import Yesod.Helpers.Form2
import Yesod.Helpers.Handler
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
-- {{{1
simpleFormPageWidgetEither = simpleFormPageWidgetEither' "POST"
-- }}}1


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
  action <- case action' of
                  Right x -> return x
                  Left r -> do
                    url_render <- getUrlRender
                    return $ url_render r
  [whamlet|
<div .alert .alert-warning>
  $maybe err_msg <- m_err_msg
     <div .form-group>
       <span .err_msg>_{err_msg}

  $if not (null overall_errors)
    <ul>输入数据错误
      $forall (_, errs) <- overall_errors
        <li>#{intercalate ";" errs}

      $forall ((_name, fs), errs) <- other_errors
        <li> _{fsLabel fs}: #{intercalate ";" errs}

<form method=#{decodeUtf8 method} action="#{action}" enctype=#{formEnctype} .form-horizontal>
  ^{formWidget}
  <div .form-group>
    <div .submit-btn-container .col-xs-offset-3 .col-xs-9>
      <input .btn .btn-primary type=submit value=_{MsgSubmitForm}>
  |]
  where error_list = fieldErrorsToList form_errs
        (overall_errors, other_errors) = partition ((== overallFieldName) . fst . fst) error_list
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

-- vim: set foldmethod=marker:
