{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Yesod.Helpers.Handler where

import Prelude
import Yesod
import Yesod.Core.Types                     (HandlerT(..), handlerRequest)
import qualified Control.Monad.Trans.Reader as R
import qualified Data.Text                  as T
import Control.Monad.Trans.Maybe

import Network.Wai                          (requestHeaders)
import Data.Time                            (UTCTime)
import Text.Blaze                           (Markup)
import Control.Monad.Catch                  (MonadThrow)
import Yesod.Helpers.Form                   (jsonOrHtmlOutputForm')
import Data.Text                            (Text)
import Data.List                            (findIndex, sortBy)
import Data.Ord                             (comparing)
import Data.Maybe                           (listToMaybe, catMaybes)
#if MIN_VERSION_base(4,8,0)
import Data.Monoid                          ((<>))
#else
import Control.Applicative                  (Applicative(..), (<|>))
import Data.Monoid                          ((<>), Monoid)
#endif
import Data.String                          (IsString)
import Network.HTTP.Types.Status            (mkStatus)


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
    -> ([Text] -> FormHandlerT site IO Html)
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
    -> ([Text] -> FormHandlerT site IO Html)
                                    -- ^ show html page function
    -> (r -> HandlerT site IO (Either [msg] TypedContent))
                                    -- ^ function to handler success form result
    -> HandlerT site IO TypedContent
runFormPostHandlerJH form_func show_page h_ok = do
    runFormPostHandler form_func $
        jsonOrHtmlOutputFormHandleResult show_page h_ok


jsonOrHtmlOutputFormX :: Yesod site =>
    ([Text] -> FormHandlerT site IO Html)
    -> [Text]
    -> FormHandlerT site IO TypedContent
jsonOrHtmlOutputFormX show_form errs = do
    (formWidget, formEnctype) <- R.ask
    let show_form' = R.runReaderT (show_form errs) (formWidget, formEnctype)
    lift $ jsonOrHtmlOutputForm' show_form' formWidget [ "form_errors" .= errs ]

jsonOrHtmlOutputFormHandleResult ::
    (Yesod site, RenderMessage site msg) =>
    ([Text] -> FormHandlerT site IO Html)
    -> (r -> HandlerT site IO (Either [msg] TypedContent))
    -> FormResult r
    -> FormHandlerT site IO TypedContent
jsonOrHtmlOutputFormHandleResult show_page f result = do
    let showf errs = jsonOrHtmlOutputFormX show_page errs
        showf' msgs = lift getMessageRender >>= showf . flip map msgs
    case result of
        FormMissing         -> showf ([] :: [Text])
        FormFailure errs    -> showf errs
        FormSuccess x       -> (lift $ f x) >>= either showf' return


matchMimeType :: ContentType -> ContentType -> Bool
matchMimeType expected actual =
    (mainType == "*" || mainType0 == "*" || mainType == mainType0)
        && (subType == "*" || subType0 == "*" || subType == subType0)
    where
        (mainType, subType) = contentTypeTypes actual
        (mainType0, subType0) = contentTypeTypes expected


lookupReqAccept :: MonadHandler m => [(ContentType -> Bool, a)] -> m (Maybe a)
lookupReqAccept lst = do
    fmap reqAccept getRequest >>= return . tryAccepts
    where
        tryAccepts cts = listToMaybe $ map snd $
                            sortBy (comparing $ fst) $ catMaybes $
                                flip map lst $
                                    \(f, x) ->
                                        fmap (, x) $ findIndex f cts


-- | runFormGet needs a special parameter "_hasdata",
-- if it does not exist, runFormGet consider no form data input at all.
-- Use this function to workaround this.
withHasDataGetParam :: HandlerT site m a -> HandlerT site m a
withHasDataGetParam f = withAlteredYesodRequest add_has_data_req f
    where
        getKey = "_hasdata"     -- 这个值 Yesod 没有 export 出来

        add_has_data_req req =
                req { reqGetParams = add_has_data $ reqGetParams req }

        add_has_data ps = maybe ((getKey, "") : ps) (const ps) $
                                lookup getKey ps


-- | modify YesodRequest before executing inner Handler code
withAlteredYesodRequest ::
    (YesodRequest -> YesodRequest)
    -> HandlerT site m a -> HandlerT site m a
withAlteredYesodRequest change f = HandlerT $ unHandlerT f . modify_hd
    where
        modify_hd hd = hd { handlerRequest = change $ handlerRequest hd }


-- | like FormResult, but used when you want to manually validate get/post params
data ParamResult a = ParamError [(Text, Text)]
                    | ParamSuccess a
                    deriving (Eq, Show)


instance Functor ParamResult where
    fmap _ (ParamError errs)    = ParamError errs
    fmap f (ParamSuccess x)     = ParamSuccess (f x)


instance Applicative ParamResult where
    pure = ParamSuccess

    (ParamError errs) <*> (ParamError errs2)    = ParamError (errs ++ errs2)
    (ParamError errs) <*> (ParamSuccess _)      = ParamError errs
    (ParamSuccess _) <*> (ParamError errs)      = ParamError errs
    (ParamSuccess f) <*> (ParamSuccess x)       = ParamSuccess $ f x


instance Monad ParamResult where
    return = pure

    (ParamError errs) >>= _ = ParamError errs
    (ParamSuccess x)  >>= f = f x

    fail msg = ParamError [("", T.pack msg)]


httpErrorRetryWithValidParams ::
    (IsString s, Monoid s, ToTypedContent s, MonadHandler m) =>
    s -> m a
httpErrorRetryWithValidParams msg = do
    sendResponseStatus (mkStatus 449 "Retry With") $
        "Retry with valid parameters: " <> msg

httpErrorWhenParamError :: MonadHandler m => ParamResult a -> m a
httpErrorWhenParamError (ParamSuccess x)    = return x
httpErrorWhenParamError (ParamError errs)   =
    httpErrorRetryWithValidParams msg
    where
        msg = T.intercalate ", " $
                flip map errs $ \(param_name, err_msg) ->
                    if T.null err_msg
                        then param_name
                        else param_name <> "(" <> err_msg <> ")"


mkParamErrorNoMsg :: Text -> ParamResult a
mkParamErrorNoMsg p = ParamError [ (p, "") ]

reqGetParamE :: MonadHandler m => Text -> m (ParamResult Text)
reqGetParamE p = fmap (maybe (mkParamErrorNoMsg p) ParamSuccess) $ lookupGetParam p

reqGetParamE' :: MonadHandler m => Text -> m (ParamResult Text)
reqGetParamE' p = reqGetParamE p >>= return . nonEmptyParam p

reqPostParamE :: MonadHandler m => Text -> m (ParamResult Text)
reqPostParamE p = fmap (maybe (mkParamErrorNoMsg p) ParamSuccess) $ lookupPostParam p

reqPostParamE' :: MonadHandler m => Text -> m (ParamResult Text)
reqPostParamE' p = reqPostParamE p >>= return . nonEmptyParam p

nonEmptyParam :: Text -> ParamResult Text -> ParamResult Text
nonEmptyParam pn pr = do
    x <- pr
    if T.null x
        then ParamError [(pn, "must not be empty")]
        else ParamSuccess x

paramErrorFromEither :: Text -> Either Text a -> ParamResult a
paramErrorFromEither pn (Left err)  = ParamError [(pn, err)]
paramErrorFromEither _  (Right x)   = ParamSuccess x

validateParam :: Text -> (a -> Either Text b) -> ParamResult a -> ParamResult b
validateParam pn f pr = do
    pr >>= paramErrorFromEither pn . f


-- | 用于保持某些 get/post 变量，以便在 redirect 和 提交表单时可以传送給下一个服务器地址
retainHttpParams :: MonadHandler m => [Text] -> m [(Text, Text)]
retainHttpParams param_names = do
    fmap catMaybes $ mapM f param_names
    where
        f n = runMaybeT $ do
                val <- MaybeT (lookupPostParam n) <|> MaybeT (lookupGetParam n)
                return (n, val)
