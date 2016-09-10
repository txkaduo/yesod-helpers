{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.Widget where

import ClassyPrelude.Yesod

import Yesod.Helpers.Message
import Yesod.Helpers.Form2


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
simpleFormPageWidget :: (MonadIO m, MonadThrow m, ToWidget master w
                        , MonadBaseControl IO m
                        , RenderMessage master a
                        , RenderMessage master YHCommonMessage
                        )
                     => (w, Enctype)
                     -> Route master  -- ^ action url
                     -> Maybe a       -- ^ any global error message
                     -> FieldErrors master  -- ^ error messages for form fields
                     -> WidgetT master m ()
simpleFormPageWidget (formWidget, formEnctype) action m_err_msg form_errs = do
  [whamlet|
$maybe err_msg <- m_err_msg
   <div .form-group>
     <span .err_msg>_{err_msg}
<ul>
  $forall ((_name, fs), errs) <- fieldErrorsToList form_errs
    <li>_{fsLabel fs}: #{intercalate ";" errs}
<form method=post action="@{action}" enctype=#{formEnctype}>
  ^{formWidget}
  <input .btn .btn-primary type=submit value=_{MsgSubmitForm}>
  |]
