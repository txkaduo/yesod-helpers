{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.Handler where

import Prelude
import Yesod
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Aeson.Types           as A

import Network.Wai                          (requestHeaders)
import Data.Time                            (UTCTime)
import Text.Blaze                           (Markup)
import Control.Monad.Catch                  (MonadThrow)
import Yesod.Helpers.Form                   (jsonOrHtmlOutputForm')
import Data.Text                            (Text)


setLastModified :: UTCTime -> HandlerT site IO ()
setLastModified = addHeader "Last-Modified" . formatRFC1123

isXHR :: (MonadHandler m) => m Bool
isXHR = do
    req <- waiRequest
    let req_with = lookup "X-Request-With" $ requestHeaders req
    return $ maybe False (== "XMLHTTPRequest") req_with


-- | form function type synonym
type MkForm site m a = Markup -> MForm (HandlerT site m) (FormResult a, WidgetT site m ())


type FormHandlerT site m a = R.ReaderT (WidgetT site m (), Enctype) (HandlerT site m) a
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
    (formWidget, formEnctype) <- generateFormPost form_func
    R.runReaderT fh (formWidget, formEnctype)

generateFormPostHandlerJH ::
    ( Yesod site, RenderMessage site FormMessage ) =>
    MkForm site IO r
    -> FormHandlerT site IO Html
    -> HandlerT site IO TypedContent
generateFormPostHandlerJH form_func show_page = do
    generateFormPostHandler form_func $ do
        jsonOrHtmlOutputFormX show_page []

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
    (RenderMessage site FormMessage, RenderMessage site msg, Yesod site) =>
    MkForm site IO r                 -- ^ form function
    -> FormHandlerT site IO Html    -- ^ show html page function
    -> (r -> HandlerT site IO (Either [msg] TypedContent))
                                    -- ^ function to handler success form result
    -> HandlerT site IO TypedContent
runFormPostHandlerJH form_func show_page h_ok = do
    runFormPostHandler form_func $
        jsonOrHtmlOutputFormHandleResult show_page h_ok


jsonOrHtmlOutputFormX :: Yesod site =>
    FormHandlerT site IO Html
    -> [A.Pair]
    -> FormHandlerT site IO TypedContent
jsonOrHtmlOutputFormX show_form other_data = do
    (formWidget, formEnctype) <- R.ask
    let show_form' = R.runReaderT show_form (formWidget, formEnctype)
    lift $ jsonOrHtmlOutputForm' show_form' formWidget other_data

jsonOrHtmlOutputFormHandleResult ::
    (Yesod site, RenderMessage site msg) =>
    FormHandlerT site IO Html
    -> (r -> HandlerT site IO (Either [msg] TypedContent))
    -> FormResult r
    -> FormHandlerT site IO TypedContent
jsonOrHtmlOutputFormHandleResult show_page f result = do
    let showf errs = jsonOrHtmlOutputFormX show_page [ "form_errors" .= errs ]
        showf' msgs = lift getMessageRender >>= showf . flip map msgs
    case result of
        FormMissing         -> showf ([] :: [Text])
        FormFailure errs    -> showf errs
        FormSuccess x       -> (lift $ f x) >>= either showf' return
