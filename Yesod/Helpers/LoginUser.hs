{-# LANGUAGE ScopedTypeVariables #-}
module Yesod.Helpers.LoginUser
  ( module Yesod.Helpers.LoginUser
  , loginParamNames, loginMsgParamName, returnUrlParamName, returnUrl2ParamName
  ) where

import ClassyPrelude.Yesod hiding (Proxy)
import qualified Network.HTTP.Types         as H
import Data.Proxy
import Data.Typeable                        (Typeable)
import Data.Time                            (TimeZone)
import Control.Monad.Catch                  (MonadThrow)

import Yesod.Helpers.Json                   (jsonDecodeKey, jsonEncodeKey)
import Yesod.Helpers.Form                   (nameToFs)
import Yesod.Helpers.Handler                (lookupReqAccept, matchMimeType, retainHttpParams)
import Yesod.Helpers.ParamNames

-- | Any logged-in user on Web
-- 本意是要打算支持多种类型用户互相独立地登录
-- 但在 Yesod 提供的机制中，一个 Yesod 实例只有一个登录的 Route，
-- 不能依据 isAuthorized 所测试的 Route 而变。
-- 曾经使用过把要登录的用户类型记录在 session 里，
-- 让负责登录的 Route 取出加以选择。
-- 这种方法并不优雅。
class
    (PersistEntity u, Typeable u
#if MIN_VERSION_persistent(2, 0, 0)
    , ToJSON (BackendKey (PersistEntityBackend u))
    , ToBackendKey (PersistEntityBackend u) u
#endif
    ) => LoginUser site u where

    -- | 对应此种用户的 session key
    loginIdentSK :: site
                 -> Proxy u
                 -> Text

    loginIdentToKey :: site
                    -> Proxy u
                    -> Text       -- ^ 在 session 中保存的用户标识字串
                    -> Maybe (Key u)
    loginIdentToKey _ _ ident = either (const Nothing) Just $ jsonDecodeKey ident

    -- | we often need time-zone to display date/time
    loginUserTimeZone :: site -> u -> TimeZone



getLoggedInUser :: forall u site.
    ( LoginUser site u
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend u (YesodPersistBackend site)
#else
    , PersistEntityBackend u ~ YesodPersistBackend site
#endif

    , PersistStore (YesodPersistBackend site)
#else
    , PersistEntityBackend u ~ PersistMonadBackend (YesodDB site)
    , PersistStore (YesodDB site)
#endif
    , YesodPersist site
    ) =>
    HandlerT site IO (Maybe (Entity u))
getLoggedInUser = do
    let mu = Proxy :: Proxy u
    maybe_key <- getLoggedInId mu
    case maybe_key of
        Nothing -> return Nothing
        Just k -> runDB $ liftM (fmap $ Entity k) $ get k

getLoggedInIdent :: (LoginUser site u, MonadHandler m, HandlerSite m ~ site)
                 => Proxy u
                 -> m (Maybe Text)
getLoggedInIdent mu = do
    foundation <- getYesod
    let sk = loginIdentSK foundation mu

    lookupSession sk


getLoggedInId :: (LoginUser site u, MonadHandler m, HandlerSite m ~ site)
              => Proxy u -> m (Maybe (Key u))
getLoggedInId mu = do
  foundation <- getYesod
  liftM (join . (fmap $ loginIdentToKey foundation mu)) $ getLoggedInIdent mu


-- | 在 session 中标记某个用户为登录状态
markLoggedIn :: forall u site . (LoginUser site u)
             => Key u -> HandlerT site IO ()
markLoggedIn uid = do
    foundation <- getYesod
    let sk = loginIdentSK foundation (Proxy :: Proxy u)
    setSession sk $ jsonEncodeKey uid


markLoggedOut :: (LoginUser site u) => Proxy u -> HandlerT site IO ()
markLoggedOut mu = do
    foundation <- getYesod
    let sk = loginIdentSK foundation mu
    deleteSession sk


data LoggedInHandler u site m a =
            LoggedInHandler
                (forall msg. RenderMessage site msg => msg -> HandlerT site m a)
                    -- a function to do "redirect"
                    -- when login is required
                (Entity u -> HandlerT site m a)
                    -- the real handler function

runLoggedInHandler ::
    ( LoginUser site u
    , RenderMessage site message
#if MIN_VERSION_persistent(2, 0, 0)

#if MIN_VERSION_persistent(2, 5, 0)
    , PersistRecordBackend u (YesodPersistBackend site)
#else
    , PersistEntityBackend u ~ YesodPersistBackend site
#endif

    , PersistStore (YesodPersistBackend site)
#else
    , PersistEntityBackend u ~ PersistMonadBackend (YesodDB site)
    , PersistStore (YesodDB site)
#endif
    , YesodPersist site
    ) =>
    message -> LoggedInHandler u site IO a -> HandlerT site IO a
runLoggedInHandler msg (LoggedInHandler rdr h) = do
    mu <- getLoggedInUser
    case mu of
        Just eu -> h eu
        Nothing -> do
            -- 因为 selectRep 一定要返回一个确定的类型 TypedContent
            -- 无法用它来实现返回其它类型的返回值，因此要自己实现类似于
            -- selectRep 的功能
            -- 以下代码就是参考 selectRep 代码写的
            -- the content types are already sorted by q values
            -- which have been stripped
            join $ fmap (fromMaybe html) $
                lookupReqAccept [ (matchMimeType "application/json", deny)
                                , (matchMimeType "text/html", html)
                                ]
    where
        html = rdr msg

        deny = do
            mr <- getMessageRender
            sendResponseStatus H.forbidden403 (mr msg)


-- | 一种常用的登录重定向方式
-- 用 query-string 传递以下的参数:
-- login_msg: 登录的提示语
-- return_url: 登录成功后应回到的位置
--
-- 没有使用 setMessage setUltDest 是为了减少使用 session
-- Yesod 的 session 内容不宜太多，因全部 session 都直接加密保存于 client
redirectToLoginRoute ::
    ( RenderMessage site message
    , MonadIO m
    , MonadThrow m
    , MonadBaseControl IO m
    ) =>
    Route site -> message -> HandlerT site m a
redirectToLoginRoute login_route msg = do
    url_render_p <- getUrlRenderParams
    m_current_r <- getCurrentRoute
    mr <- getMessageRender
    req <- getRequest
    redirect $ url_render_p login_route $
        (loginMsgParamName, mr msg) : case m_current_r of
                                    Nothing -> []
                                    Just r -> [ (returnUrlParamName, url_render_p r (reqGetParams req)) ]

data LoginParams = LoginParams {
                    loginParamFromUrl   :: Maybe Text
                    , loginParamMessage :: Maybe Text
                    }

getLoginParam :: (MonadIO m, MonadThrow m, MonadBaseControl IO m) =>
    HandlerT site m LoginParams
getLoginParam = do
    u <- lk returnUrlParamName
    msg <- lk loginMsgParamName
    return $ LoginParams u msg
    where
        lk x = lookupPostParam x >>= maybe (lookupGetParam x) (return . Just)

loginParamHiddenFormParts ::
    -- (Monad m, PathPiece p, RenderMessage (HandlerSite m) FormMessage) =>
    (MonadIO m, MonadBaseControl IO m, MonadThrow m
    , RenderMessage site FormMessage) =>
    MForm (HandlerT site m) [(FormResult (Maybe Text), FieldView site)]
loginParamHiddenFormParts = do
    lp <- lift getLoginParam
    sequence
        [ mopt hiddenField (nameToFs returnUrlParamName) (Just $ loginParamFromUrl lp)
        , mopt hiddenField (nameToFs loginMsgParamName) (Just $ loginParamMessage lp)
        ]

getLoginUrlRender :: MonadHandler m => m (Route (HandlerSite m) -> Text)
getLoginUrlRender = do
  flip <$> getUrlRenderParams <*> retainHttpParams loginParamNames

