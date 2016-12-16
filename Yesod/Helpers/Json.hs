{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.Json where

import ClassyPrelude.Yesod
import Control.Monad.Writer (Writer)
import qualified Data.Text.Encoding         as TE
import qualified Data.ByteString.Lazy       as LB
import qualified Data.Aeson.Parser          as AP
import qualified Data.Aeson                 as A
import Data.Aeson.Types                     (Pair, parseEither)
import Data.Attoparsec.ByteString           (parseOnly)
import Data.Monoid (Endo)
import Text.Julius (Javascript)


import Yesod.Helpers.Handler                (lookupReqAccept, matchMimeType)
import Yesod.Helpers.JSend


jsonErrorOutput' :: (ToJSON a) => a -> [ (Text, Value) ]
jsonErrorOutput' msg = [ "message" .= msg, "result" .= ("fail" :: Text) ]

jsonErrorOutput :: (ToJSON a) => a -> Value
jsonErrorOutput msg = object $ jsonErrorOutput' msg

jsonOkOutput' :: (ToJSON a) => a -> [ (Text, Value) ]
jsonOkOutput' msg = [ "message" .= msg, "result" .= ("success" :: Text) ]

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


-- | if client accept json (ajax call), give a http 401 status code, and a Location header
-- otherwise, just a normal http redirect 301
redirectOrJson401 ::
     (MonadHandler m, RedirectUrl (HandlerSite m) url) =>
    url -> m a
redirectOrJson401 route = do
    is_ajax <- acceptsJson
    if is_ajax
       then do
           url <- toTextUrl route
           addHeader "Location" url
           sendResponseStatus unauthorized401 ()
        else
            redirect route


provideRepRedirectOrJs401 :: forall m url. (MonadHandler m, RedirectUrl (HandlerSite m) url)
                          => url
                          -> Writer (Endo [ProvidedRep m]) ()
provideRepRedirectOrJs401 url = do
  provideRep $ (redirectOrJson401 url :: m JSendMsg)
  provideRep $ (redirectOrJson401 url :: m Javascript)
  provideRep $ (redirect url :: m Html)


-- | 把一个 Key 用JSON字串表示
jsonEncodeKey ::
#if MIN_VERSION_persistent(2, 0, 0)
    ( ToJSON (BackendKey backend)
    , ToBackendKey backend a
    ) =>
#endif
    Key a -> Text
jsonEncodeKey = TE.decodeUtf8 . LB.toStrict . A.encode .
#if MIN_VERSION_persistent(2, 0, 0)
                    toBackendKey
#else
                    unKey
#endif

jsonDecodeKey ::
#if MIN_VERSION_persistent(2, 0, 0)
    ( ToBackendKey backend a
#if MIN_VERSION_persistent(2, 5, 0)
#else
    , backend ~ PersistEntityBackend a
#endif

    ) =>
#endif
    Text -> Either String (Key a)
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
        k' = unpack k

getOrRedirectJH ::
    ( YesodPersist site
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , YesodPersistBackend site ~ PersistEntityBackend val
    , PersistEntity val
#endif

    , PersistStore (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistStore (YesodPersistBackend site (HandlerT site IO))
#endif
    , RenderMessage site message
    , RedirectUrl site url
    ) =>
    message -> url -> Key val -> HandlerT site IO val
getOrRedirectJH msg route key = do
    (runDB $ get key) >>= maybe (redirectOrJson msg route) return

getByOrRedirectJH ::
    ( YesodPersist site
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend val (YesodPersistBackend site)
#else
    , YesodPersistBackend site ~ PersistEntityBackend val
    , PersistEntity val
#endif

    , PersistUnique (YesodPersistBackend site)
#else
    , PersistMonadBackend (YesodDB site) ~ PersistEntityBackend val
    , PersistUnique (YesodPersistBackend site (HandlerT site IO))
#endif
    , RenderMessage site message
    , RedirectUrl site url
    ) =>
    message -> url -> Unique val -> HandlerT site IO (Entity val)
getByOrRedirectJH msg route u_key = do
    (runDB $ getBy u_key) >>= maybe (redirectOrJson msg route) return
