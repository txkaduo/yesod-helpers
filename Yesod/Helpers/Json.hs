{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Json where

import Yesod
import qualified Data.Aeson.Types           as A
import Data.Text                            (pack, Text)


jsonErrorOutput' :: (ToJSON a) => a -> [ (Text, Value) ]
jsonErrorOutput' msg = [ "message" .= msg, "result" .= pack "fail" ]

jsonErrorOutput :: (ToJSON a) => a -> Value
jsonErrorOutput msg = object $ jsonErrorOutput' msg

jsonOkOutput' :: (ToJSON a) => a -> [ (Text, Value) ]
jsonOkOutput' msg = [ "message" .= msg, "result" .= pack "success" ]

jsonOkOutput :: (ToJSON a) => a -> Value
jsonOkOutput msg = object $ jsonOkOutput' msg

jsonErrorOutputData :: (ToJSON a) => a -> Value -> Value
jsonErrorOutputData msg jv = object $ ("data" .= jv) : obj0
    where obj0 = jsonErrorOutput' msg

jsonOkOutputData :: (ToJSON a) => a -> Value -> Value
jsonOkOutputData msg jv = object $ ("data" .= jv) : obj0
    where obj0 = jsonOkOutput' msg

-- | 如果是普通页面输出，则，调用 setMessageI 并重定向至指定的 route
-- 如果是 JSON 输出，则按操作成功的方式输出文字信息
succeedOutputJsonOrRedirect ::
    (RenderMessage site message, RedirectUrl site url) =>
    message -> url -> HandlerT site IO TypedContent
succeedOutputJsonOrRedirect msg route = do
    succeedOutputJsonDataOrRedirect msg route []

succeedOutputJsonDataOrRedirect ::
    forall site message url.
    (RenderMessage site message, RedirectUrl site url) =>
    message -> url -> [A.Pair] -> HandlerT site IO TypedContent
succeedOutputJsonDataOrRedirect msg route other_data = do
    selectRep $ do
        provideRep $ do
            msgRender <- getMessageRender
            let tmsg = msgRender msg
            turl <- toTextUrl route
            let dat = object $ ( "url" .= turl ) : other_data
            return $ jsonOkOutputData tmsg dat
        provideRep $ html 
        where
            html :: HandlerT site IO Html
            html = do
                setMessageI $ msg
                redirect route

errorOutputJsonOrHtml ::
    (MonadHandler m, HasContentType a,
     RenderMessage (HandlerSite m) message) =>
    message -> m a -> m TypedContent
errorOutputJsonOrHtml msg showf = do
    msgRender <- getMessageRender
    let tmsg = msgRender msg
    selectRep $ do
        provideRep $ do
            setMessage $ toHtml $ tmsg
            showf
        provideRep $ do
            return $ jsonErrorOutput tmsg
