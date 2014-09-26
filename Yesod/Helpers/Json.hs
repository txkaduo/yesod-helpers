{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
module Yesod.Helpers.Json where

import Yesod
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Aeson.Parser          as AP
import qualified Data.Aeson                 as A
import Data.Aeson.Types                     (Pair, parseEither)
import Data.Attoparsec.ByteString           (parseOnly)
import Data.Maybe                           (fromMaybe)
import Control.Monad                        (join)
import Data.Text                            (pack, Text)
import qualified Data.Text                  as T


import Yesod.Helpers.Handler                (lookupReqAccept, matchMimeType)

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
    message -> url -> [Pair] -> HandlerT site IO TypedContent
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


redirectOrJson ::
     (RenderMessage (HandlerSite m) message, MonadHandler m
     , RedirectUrl (HandlerSite m) url
     ) =>
    message -> url -> m a
redirectOrJson msg route = do
    msgRender <- getMessageRender
    let tmsg = msgRender msg
    join $ fmap (fromMaybe $ json tmsg) $
        lookupReqAccept [ (not . matchMimeType "application/json", rdr) ]
    where
        json tmsg = sendResponse $ jsonErrorOutput tmsg
        rdr = redirect route

-- | 把一个 Key 用JSON字串表示
jsonEncodeKey :: Key a -> Text
jsonEncodeKey = TE.decodeUtf8 . LB.toStrict . A.encode . unKey

jsonDecodeKey :: Text -> Either String (Key a)
jsonDecodeKey k =
    case parseOnly AP.value (TE.encodeUtf8 k) of
        Left err ->
            Left $ "cannot parse string as JSON: " ++ err ++ ", k=" ++ k'
        Right jval ->
            case parseEither A.parseJSON jval of
                Left e ->
                    Left $ "cannot parse string as PersistValue: " ++ e ++ ", k=" ++ k'
                Right pv -> Right pv
    where
        k' = T.unpack k

getOrRedirectJH ::
    ( PersistEntity val
    , YesodPersist site
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , RenderMessage site message
    , RedirectUrl site url
    , PersistStore (YesodPersistBackend site (HandlerT site IO))
    ) =>
    message -> url -> Key val -> HandlerT site IO val
getOrRedirectJH msg route key = do
    (runDB $ get key) >>= maybe (redirectOrJson msg route) return

getByOrRedirectJH ::
    ( PersistEntity val
    , YesodPersist site
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , RenderMessage site message
    , RedirectUrl site url
    , PersistUnique (YesodPersistBackend site (HandlerT site IO))
    ) =>
    message -> url -> Unique val -> HandlerT site IO (Entity val)
getByOrRedirectJH msg route u_key = do
    (runDB $ getBy u_key) >>= maybe (redirectOrJson msg route) return
