module Yesod.Helpers.Widget where

import ClassyPrelude.Yesod

import Yesod.Helpers.Message
import Yesod.Helpers.Form2
import Yesod.Helpers.Handler


-- | construct page title by merging pieces of texts, separated by MsgPageTitleSep
mergePageTitles :: ( RenderMessage (HandlerSite m) message
                    , RenderMessage (HandlerSite m) YHCommonMessage
                    , MonadWidget m
                    )
                    => [message]
                    -> m ()
mergePageTitles parts = do
    mr <- getMessageRender
    mr2 <- getMessageRender
    setTitleI $ mconcat $ intersperse (mr MsgPageTitleSep) $ map mr2 parts


-- | A simple widget to show a html form
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
simpleFormPageWidgetEither (formWidget, formEnctype) action' m_err_msg form_errs = do
  action <- case action' of
                  Right x -> return x
                  Left r -> do
                    url_render <- getUrlRender
                    return $ url_render r
  [whamlet|
$maybe err_msg <- m_err_msg
   <div .form-group>
     <span .err_msg>_{err_msg}
<ul>
  $forall ((_name, fs), errs) <- fieldErrorsToList form_errs
    <li>_{fsLabel fs}: #{intercalate ";" errs}
<form method=post action="#{action}" enctype=#{formEnctype} .form-horizontal>
  ^{formWidget}
  <div .form-group>
    <div .submit-btn-container>
      <input .btn .btn-primary type=submit value=_{MsgSubmitForm}>
  |]


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
