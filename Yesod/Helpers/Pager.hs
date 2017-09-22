module Yesod.Helpers.Pager where

-- {{{1 imports
import ClassyPrelude.Yesod

import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List        as CL
-- }}}1


data PagerSettings = PagerSettings
  { pagerNumPerPage   :: Int
  , pagerPageNumParam :: Text
  }


data PagedResult = PagedResult
  { pagedFirstPageUrl   :: Text
  , pagedLastPageUrl    :: Text
  , pagedCurrentPageNum :: Int
  , pagedLastPageNum    :: Int
  , pagedPrevPageUrl    :: Maybe Text
  , pagedNextPageUrl    :: Maybe Text
  , pagedMakePageUrl    :: Int -> Text
  }


jsonPagedResult :: PagedResult -> Value
jsonPagedResult x = object [ "first_page_url" .= pagedFirstPageUrl x
                           , "last_page_url" .= pagedLastPageUrl x
                           , "current_page_num" .= pagedCurrentPageNum x
                           , "last_page_num" .= pagedLastPageNum x
                           , "prev_page_url" .= pagedPrevPageUrl x
                           , "next_page_url" .= pagedNextPageUrl x
                           ]


pagerGetCurrentPageNumGet :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => PagerSettings -> m Int
pagerGetCurrentPageNumGet (PagerSettings { pagerPageNumParam = pn_param }) =
  fmap (max 1 . fromMaybe 1) $ runInputGet $ iopt intField pn_param


pagerGetCurrentPageNumPost :: (MonadHandler m, RenderMessage (HandlerSite m) FormMessage) => PagerSettings -> m Int
pagerGetCurrentPageNumPost (PagerSettings { pagerPageNumParam = pn_param }) =
  fmap (max 1 . fromMaybe 1) $ runInputPost $ iopt intField pn_param

pagerGetOffset :: PagerSettings -> Int -> Int
pagerGetOffset (PagerSettings { pagerNumPerPage = npp } ) pn = (pn -1) * npp

pagerSelectOpts :: PagerSettings -> Int -> [SelectOpt a]
pagerSelectOpts ps pn = [ OffsetBy (pagerGetOffset ps pn), LimitTo (pagerNumPerPage ps) ]

pagerWidget :: (MonadIO m, MonadThrow m, MonadBaseControl IO m)
            => PagedResult -> Int -> WidgetT site m ()
pagerWidget paged total_num = [whamlet|$newline never
  <div>
    #{tshow $ pagedCurrentPageNum paged}/#{tshow $ pagedLastPageNum paged}页, 共#{tshow total_num}条记录
  $if pagedLastPageNum paged > 1
    <ul .pager>
      <li>
        <a href="#{pagedFirstPageUrl paged}">首页
      $maybe url <- pagedPrevPageUrl paged
        <li>
          <a href="#{url}">前一页
      $maybe url <- pagedNextPageUrl paged
        <li>
          <a href="#{url}">后一页
      <li>
        <a href="#{pagedLastPageUrl paged}">末页
          |]

runPager :: (MonadHandler m)
         => PagerSettings
         -> Int
         -> Int
         -> m PagedResult
-- {{{1
runPager (PagerSettings npp pn_param) pn total_num = do
  params0 <- reqGetParams <$> getRequest
  let params = foldr deleteMap params0 exclude_params

  current_route <- fromMaybe (error "getCurrentRoute failed") <$> getCurrentRoute

  url_render <- getUrlRenderParams

  let link_to_pn p = url_render current_route $ insertMap pn_param (tshow p) params
      first_page_url = link_to_pn (1 :: Int)
      last_page_num = (total_num + npp -1) `div` npp
      last_page_url = link_to_pn last_page_num
      prev_page_url = if pn > 1
                         then Just $ link_to_pn (pn -1)
                         else Nothing
      next_page_url = if pn < last_page_num
                         then Just $ link_to_pn (pn + 1)
                         else Nothing

  return $ PagedResult
            first_page_url
            last_page_url
            pn
            last_page_num
            prev_page_url
            next_page_url
            link_to_pn

  where
    exclude_params = [-- "_hasdata"  -- a special param name used in yesod internally
                      -- _hasdata 表示当前参数属于某GET表单，移除的话该表单将不能工作
                      ]
-- }}}1



-- | 把输入的数据按分页的参数取出一小部分列表，并得到总数
-- CAUTION: 这通常使用数据库 SQL 的分页参数是更好的替代做法．
--          但这个做法的好处是:
--          * 代码看上去简单些．使用 SQL 方法要么发起多一次 count(*) 的查询，
--            要么要使用比较复杂的SQL语句（要使用rawSql）
--          * 有时候数据源并不一句SQL语句得到，而是多个查询的组合，难以使用SQL方法分页
sinkPagedListCount :: Monad m
                   => Int
                   -> Int
                   -> Sink a m ([a], Int)
-- {{{1
sinkPagedListCount npp0 pn0 =
  getZipSink $ (,) <$> ZipSink (CL.drop ((pn -1) * npp) >> CL.take npp)
                   <*> ZipSink CC.length
  where
    pn = max 1 pn0
    npp = max 1 npp0
-- }}}1

class NumPerPage a where
  numPerPage :: a -> Int



-- vim: set foldmethod=marker:
