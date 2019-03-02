module Yesod.Helpers.Utils where

-- {{{1
import ClassyPrelude
import Control.Monad.STM                    (check)
import Data.Char
import qualified Data.Map                   as Map
import Data.List                            ((!!))
import Data.Proxy
import qualified Data.Text                  as T
import qualified Data.Sequence              as Sq
import Data.Time                            ( localTimeToUTC, zonedTimeToUTC, TimeZone, ParseTime
                                            , LocalTime(..), midnight, TimeOfDay, addDays
                                            , NominalDiffTime, TimeLocale(..), TimeZone(..)
                                            )
import Data.Time.Clock.POSIX                ( posixSecondsToUTCTime
                                            , utcTimeToPOSIXSeconds)
#if MIN_VERSION_time(1,5,0)
import Data.Time                            (parseTimeM)
#else
import Data.Time                            (parseTime)
#endif
import Control.Monad.Logger
import qualified Control.Monad.Logger.CallStack as LCS
import System.Random                        (randomIO)
#if !MIN_VERSION_classy_prelude(1, 0, 0)
import Control.Monad.Trans.Control          (MonadBaseControl)
#endif
import System.Timeout                       (timeout)
import Network.HTTP.Types                   (parseQueryText, renderQueryText, QueryText, urlEncode)
import qualified Network.Mime               as MM
import Network.URI                          (parseURIReference, uriQuery, uriToString
                                            , parseAbsoluteURI, uriAuthority, uriRegName
                                            , URI, uriScheme, uriPort
                                            )
import Yesod.Core.Types                     (ContentType)
import qualified Blaze.ByteString.Builder   as BBB
import qualified Data.ByteString.UTF8       as UTF8
import qualified Data.ByteString.Lazy       as LB
import qualified Data.ByteString.Base64.Lazy as LB64

import GHC.Stack
-- }}}1



-- | 用于隐藏用户名，昵称等名字
-- 保留首尾字符，中间字符用*代替
maskName :: String -> String
-- {{{1
maskName t = do
  case t of
    [] -> "*"
    (x:xs) -> case reverse xs of
                [] -> "*"
                (y:ys) -> x : replicate (length ys) '*' <> [y]
-- }}}1


maskNameT :: Text -> Text
maskNameT = pack . maskName . unpack


toHalfWidthEnglishAlpha :: Char -> Char
toHalfWidthEnglishAlpha ch
    | ch >= 'Ａ' && ch <= 'Ｚ'  = chr $ ord 'A' + ord ch - ord 'Ａ'
    | ch >= 'ａ' && ch <= 'ｚ'  = chr $ ord 'a' + ord ch - ord 'ａ'
    | otherwise                 = ch


toHalfWidthDigit :: Char -> Char
toHalfWidthDigit ch
    | ch >= '０' && ch <= '９'  = chr $ ord '0' + ord ch - ord '０'
    | otherwise                 = ch

toHalfWidthEnglishAlphaDigit :: Char -> Char
toHalfWidthEnglishAlphaDigit = toHalfWidthEnglishAlpha . toHalfWidthDigit


nullToNothing :: MonoFoldable a => a -> Maybe a
nullToNothing t = if null t
                     then Nothing
                     else Just t

{-# DEPRECATED emptyTextToNothing "use nullToNothing instead" #-}
emptyTextToNothing :: Text -> Maybe Text
emptyTextToNothing = nullToNothing

{-# DEPRECATED mapLeft "use left in Control.Arrow instead" #-}
mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft _ (Right x) = Right x
mapLeft f (Left x)  = Left (f x)


-- | parse a UTCTime, try the following format
-- 2015-01-02 12:53:46+0800
-- 2015-01-02 12:53:46 (as in the specified time zone)
-- 2015年01月02日12时53分46秒 (as in the specified time zone)
humanParseUTCTime :: TimeZone -> String -> Maybe UTCTime
humanParseUTCTime tz s =
    parse_local "%Y-%m-%d %T"
        <|> parse_local "%Y年%m月%d日%H时%M分%S秒"
        <|> parse_day "%Y-%m-%d"
        <|> parse_day "%Y年%m月%d日"
        <|> parse_zoned "%Y-%m-%d %T%z"
        <|> parse_zoned "%Y-%m-%d %T%Z"
    where
        parse_t :: ParseTime t => String -> Maybe t
        parse_t fmt =
#if MIN_VERSION_time(1,5,0)
                parseTimeM False defaultTimeLocale fmt s
#else
                parseTime defaultTimeLocale fmt s
#endif

        parse_local fmt = fmap (localTimeToUTC tz) $ parse_t fmt
        parse_zoned fmt = fmap zonedTimeToUTC $ parse_t fmt
        parse_day fmt = do
            d <- parse_t fmt
            return $ localTimeToUTC tz $ LocalTime d midnight


epochIntToUtcTime :: Int64 -> UTCTime
epochIntToUtcTime = posixSecondsToUTCTime . (realToFrac :: Int64 -> NominalDiffTime)

utcTimeToEpochInt :: UTCTime -> Int64
utcTimeToEpochInt = round . utcTimeToPOSIXSeconds


randomPick :: MonadIO m => [a] -> m a
randomPick choices = do
    idx' <- liftIO randomIO
    let idx = abs idx' `rem` chlen
    return $ choices !! idx
    where
        chlen = length choices


randomString :: MonadIO m => Int -> [Char] -> m [Char]
randomString len chars = replicateM len (randomPick chars)


randomUrlSafeString :: MonadIO m => Int -> m [Char]
randomUrlSafeString = flip randomString $ ['0'..'9'] <> ['a'..'z'] <> ['A'..'Z'] <> "-_"


zhCnTimeLocale :: TimeLocale
-- {{{1
zhCnTimeLocale =
  TimeLocale
    [ ("星期日", "周日")
    , ("星期一", "周一")
    , ("星期二", "周二")
    , ("星期三", "周三")
    , ("星期四", "周四")
    , ("星期五", "周五")
    , ("星期六", "周六")
    ]
    [ ("一月", "一月")
    , ("二月", "二月")
    , ("三月", "三月")
    , ("四月", "四月")
    , ("五月", "五月")
    , ("六月", "六月")
    , ("七月", "七月")
    , ("八月", "八月")
    , ("九月", "九月")
    , ("十月", "十月")
    , ("十一月", "十一月")
    , ("十二月", "十二月")
    ]
    ("上午", "下午")
    "%Y-%m-%d %H:%M:%S"  -- dateTimeFmt
    "%Y-%m-%d"  -- dateFmt
    "%H:%M:%S"  -- timeFmt
    "%I:%M:%S"  -- time12Fmt
    [ TimeZone 480 False "北京"
    ]
-- }}}1


-- | generate random string with 'safe' characters:
-- ASCII alpha and number only, with some characters ('l', 'o') excluded.
randomHumanSafeStr :: MonadIO m => IsString s => Int -> m s
randomHumanSafeStr slen = do
    ws <- liftIO $ replicateM slen $ randomIO
    return $ fromString $ flip map ws $
                    \w -> safe_chars !!
                        (fromIntegral (w :: Word8) `mod` length safe_chars)

    where
        -- length of safe_char is 32
        safe_chars = filter (not . (`elem` ['l', 'o'])) $ ['a' .. 'z'] ++ ['2' .. '9']


-- | Make an empty MVar, supply it to the constructor, construct a message for the Chan,
-- wait for a reply in the MVar.
writeChanWaitMVar :: (MonadIO m)
                    => Chan c           -- ^ the message channel
                    -> (MVar a -> c)    -- ^ the channel message maker
                    -> m a
writeChanWaitMVar ch mk_cmd = do
    mvar <- liftIO newEmptyMVar
    let cmd = mk_cmd mvar
    liftIO $ writeChan ch cmd
    liftIO $ takeMVar mvar


-- | 在一定时间内，等待一个 TVar 变成 True
-- TVar 的意义应该是某种结束标志
-- 所以超时代表不结束，此函数会直接返回 False
checkTVarTimeout :: MonadIO m
                 => Int
                 -> TVar Bool
                 -> m Bool
checkTVarTimeout ms tvar = liftIO $ do
  fmap (fromMaybe False) $ timeout ms $ atomically (readTVar tvar >>= check >> return True)


checkExitTVarOrElse :: TVar Bool -> STM a -> STM (Maybe a)
checkExitTVarOrElse exit_var f = do
  (readTVar exit_var >>= check >> return Nothing) <|> fmap Just f


-- | 配合 startBgThreadIdentW 及 foreverWithExitCheck 使用的小工具
logExcWithThreadIdent :: (HasCallStack, MonadLogger m) => Text -> SomeException -> m ()
-- {{{1
logExcWithThreadIdent thr_ident e = do
    if null thr_ident
       then LCS.logError $ "Got exception in loop: " <> tshow e
       else LCS.logError $ "Thread of " <> thr_ident <> ": Got exception in loop: " <> tshow e
-- }}}1


foreverUntilFalse :: Monad m => m Bool -> m Bool
foreverUntilFalse f = loop
  where loop = f >>= bool (return False) loop


-- | 重复调用工作函数，每次调用前调用一次检查是否要退出的函数
-- 结束循环的条件是以下任意一种情况成立
-- * 工作函数返回 False
-- * 检查退出函数返回 True
foreverWithExitCheck :: (MonadIO m, MonadLogger m
#if MIN_VERSION_classy_prelude(1, 0, 0)
                          , MonadCatch m
#else
                          , MonadBaseControl IO m
#endif
                          )
                      => (Maybe SomeException -> m Bool)
                      -- ^ This function should return True if need to exit between iterations.
                      -- It can be blocking op (timeout somehow)
                      -- Or can return result immediately
                      -- 这个函数会在工作函数正常或异常结束时调用
                      -- 即在每次重复调用工作函数前调用
                      -> m Bool
                      -- ^ 这个函数代表一轮要完成的工作
                      -- 它返回 True 代表暂时没有工作可以做，但希望再重试
                      -- 返回 False 代表全部工作已完成，不再循环
                      -> m ()
-- {{{1
foreverWithExitCheck check_exit f = loop
  where
    loop = do
      err_or_maybe_more <- fmap Right f `catchAny` (return . Left)
      let m_err = either Just (const Nothing) err_or_maybe_more
      need_exit <- check_exit m_err
      if need_exit
         then return ()
         else case err_or_maybe_more of
                Right False -> return ()
                _           -> loop
-- }}}1


class StmQueue t where
  newStmQueue :: STM (t a)
  readStmQueue :: t a -> STM a
  tryReadStmQueue :: t a -> STM (Maybe a)
  writeStmQueue :: t a -> a -> STM ()


instance StmQueue TChan where
  newStmQueue = newTChan
  readStmQueue = readTChan
  tryReadStmQueue = tryReadTChan
  writeStmQueue = writeTChan

instance StmQueue TQueue where
  newStmQueue = newTQueue
  readStmQueue = readTQueue
  tryReadStmQueue = tryReadTQueue
  writeStmQueue = writeTQueue



-- | 从原有的 StmQueue 搬数据到新的 StmQueue
-- 保证相同键值的数据之间有至少一定的时间间隔
intersperseDelayStmQueue :: (Ord b, StmQueue t1, StmQueue t2)
                         => (a -> b)
                         -> Int -- ^ 最小等待时间 ms
                         -> t1 a
                         -> IO (t2 a, (Async (), TVar Bool))
                         -- ^ 新的TChan，及用于退出生成的工作线程的工具
-- {{{1
intersperseDelayStmQueue get_key delay_ms old_chan = do
  new_chan <- atomically newStmQueue
  exit_var <- newTVarIO False
  ref_buffer <- newIORef (asMap mempty)

  let loop = do
        result <- do
          the_map <- readIORef ref_buffer
          atomically $ do
            need_exit <- readTVar exit_var
            if need_exit
               then do
                    -- 准备退出工作时，有两种策略
                    -- 一种是立即把已缓存的数据发出，以便下个环节处理
                    -- 另一种是不再读新的数据，但保持原有的等待然后处理的逻辑
                    -- 为保持尽量优雅的退出方式，后一种似乎更合理
                    -- 如果调用者想立即退出，可以 cancel 所返回的 Async
                    -- 因为不再读新的数据，缓存中若有空的列表，可以删除
                    let new_map = Map.filter (not . null . snd) the_map
                    if null new_map
                       then return Nothing
                       else fmap (Just . (new_map,) . Left) $
                              asum (map read_tvar_in_map $ mapToList new_map)

               else fmap (Just . (the_map,)) $
                      fmap Left (asum (map read_tvar_in_map $ mapToList the_map))
                        <|> fmap Right (readStmQueue old_chan)

        case result of
          Nothing -> do
            -- 所有工作已完成, 退出循环
            return ()

          Just (the_map, old_or_new) -> do
            case old_or_new of
              Left (xk, (_tv, vlist)) -> do
                -- 有一个旧的数据组到期要处理
                case Sq.viewr vlist of
                  Sq.EmptyR -> modifyIORef' ref_buffer $ deleteMap xk
                  vlist2 Sq.:> v -> do
                    -- keep save vlist2 event if it may be null.
                    -- so when new value comes in, we know we should wait
                    atomically $ writeStmQueue new_chan v
                    new_tv <- registerDelay delay_ms
                    modifyIORef' ref_buffer $ insertMap xk (new_tv, vlist2)

              Right v -> do
                -- 有新的数据进入
                let xk = get_key v
                let update_map = modifyIORef' ref_buffer . insertMap xk

                case lookup xk the_map of
                  Just (tv, vlist) -> do
                    update_map  (tv, cons v vlist)

                  Nothing -> do
                    -- no preceding element, just pass through
                    atomically $ writeStmQueue new_chan v
                    tv <- registerDelay delay_ms
                    update_map (tv, mempty)

            loop

  thr <- async loop
  return (new_chan, (thr, exit_var))
  where
    read_tvar_in_map (k, (tv, v)) = do
      b <- readTVar tv
      check b
      return (k, (tv, v))
-- }}}1


-- | 辅助读 intersperseDelayStmQueue 中的信息并处理的函数
handleReadDelayedStmQueue :: (MonadIO m, MonadLogger m, MonadBaseControl IO m, MonadMask m, Ord b, StmQueue t)
                          => TVar Bool
                          -- ^ 全局结束标志
                          -> (Maybe SomeException -> m Bool)
                          -- ^ 工作函数每次调用之间检查是否退出
                          -> (a -> b)
                          -- ^ 数据的健值
                          -> Int
                          -- ^ 最小延时 ms
                          -> t a
                          -- ^ 原始的数据输入
                          -> (a -> m ())
                          -- ^ 真正处理得到的数据的逻辑
                          -> m ()
-- {{{1
handleReadDelayedStmQueue app_exit_var check_exit_between get_key delay_ms in_chan real_work = do
  bracket
    (liftIO $ intersperseDelayStmQueue get_key delay_ms in_chan)
    cleanup
    (foreverWithExitCheck check_exit_between . loop)
      -- 最外层循环控制异常后重试

  where
    loop (new_chan, (thr, exit_var)) = do
      m_job <- atomically $ do
        app_exiting <- readTVar app_exit_var
        if app_exiting
           then writeTVar exit_var True >> tryReadTChan new_chan
           else fmap Just $ readTChan new_chan

      mapM_ real_work m_job
      if isJust m_job
         then loop (new_chan, (thr, exit_var))
         else return False

    cleanup (_new_chan, (_thr, exit_var)) = atomically $ writeTVar exit_var True
-- }}}1


urlUpdateQueryText :: (QueryText -> QueryText)
                    -> String
                    -> Maybe String
urlUpdateQueryText f s = do
    uri <- parseURIReference s
    return $ flip (uriToString id) "" $
        uri { uriQuery = (UTF8.toString $ BBB.toByteString $
                                renderQueryText True $
                                    f $ parseQueryText $ UTF8.fromString $ strip_qm $ uriQuery uri)
                }
    where
        strip_qm ('?':xs)   = xs
        strip_qm xs         = xs


queryTextSetParam :: [(Text, Maybe Text)]
                    -> QueryText
                    -> QueryText
queryTextSetParam = flip go
    where
        go ys []        = ys
        go ys (x:xs)    =   let ys' = filter ((/= fst x) . fst) ys
                            in go (ys' ++ [ x ]) xs


extractHostFromAbsUrl :: String -> Maybe String
extractHostFromAbsUrl uri = do
    puri <- parseAbsoluteURI uri
    auth <- uriAuthority puri
    return (uriRegName auth)


-- | Prepend a dot on a domain unless it already begins with a dot.
domainPrependDot :: ( IsString s
#if MIN_VERSION_mono_traversable(1, 0, 0)
                    , Eq (Element s), IsSequence s, Semigroup s
#else
                    , EqSequence s
#endif
                    )
                 => s -> s
domainPrependDot d = if isPrefixOf "." d
                        then d
                        else "." <> d


-- | Get http origin from url
uriOrigin :: URI -> String
uriOrigin uri =
  uriScheme uri <> fromMaybe "" (uri_auth_no_user <$> uriAuthority uri)
  where
    uri_auth_no_user x = "//" <> uriRegName x <> uriPort x


-- | Construct LocalTime for the start point of time range, with optional time of day.
-- if TimeOfDay specified, use 'midnight' (the start time of the day)
mkLocalTimeSince :: Day -> Maybe TimeOfDay -> LocalTime
mkLocalTimeSince d m_tod = LocalTime d (fromMaybe midnight m_tod)


-- | Construct LocalTime for the end point of time range, with optional time of day.
-- if TimeOfDay specified, use 'midnight' (the start time of the day) of the next day.
mkLocalTimeTill :: Day -> Maybe TimeOfDay -> LocalTime
mkLocalTimeTill d Nothing    = LocalTime (addDays 1 d) midnight
mkLocalTimeTill d (Just tod) = LocalTime d tod


base64DataURI :: ContentType -> LB.ByteString -> LB.ByteString
base64DataURI ct lbs =
  mconcat [ "data:"
          , fromStrict ct
          , ";base64"
          , ","
          , LB64.encode lbs
          ]


proxyOf :: a -> Proxy a
proxyOf _ = Proxy


-- | 检查字串是否是合法的中国手机号，并去除多余的前缀如+86等，去除中间的减号
normalizeChineseMobileNum :: Text -> Maybe Text
-- {{{1
normalizeChineseMobileNum = listToMaybe . chk_mobile_maybe
  where
    chk_mobile_raw t = do
        let t' = T.filter (/= '-') t
        unless (T.all isDigit t' && T.length t' == 11 && T.isPrefixOf "1" t') mzero
        return t'

    -- handle +86
    chk_mobile_plus t = do
        T.stripPrefix "+" t >>= chk_mobile_cn

    chk_mobile_cn t = do
        T.stripPrefix "86" t >>= chk_mobile_raw

    chk_mobile_maybe t = catMaybes  [ chk_mobile_cn t
                                    , chk_mobile_plus t
                                    , chk_mobile_raw t
                                    ]
-- }}}1


-- | RFC5987 encoding, with utf-8, no language tag
encodeUtf8Rfc5987 :: (Utf8 textual ByteString) => textual -> ByteString
encodeUtf8Rfc5987 t = "UTF-8''" <> urlEncode False (encodeUtf8 t)


-- | Lookup file extension for a MIME
defaultExtensionOfMime :: MM.MimeType -> Maybe MM.Extension
defaultExtensionOfMime mt = fmap fst $ find ((== mt) . snd) (mapToList MM.defaultMimeMap)


-- vim: set foldmethod=marker:
