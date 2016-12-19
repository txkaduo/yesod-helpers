module Yesod.Helpers.Pager where

-- {{{1 imports
import ClassyPrelude.Yesod
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
    exclude_params = [ "_hasdata"  -- a special param name used in yesod internally
                      ]
-- }}}1


-- vim: set foldmethod=marker:
