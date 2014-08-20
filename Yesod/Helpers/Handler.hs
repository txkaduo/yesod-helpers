{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.Handler where

import Prelude
import Yesod

import Network.Wai                          (requestHeaders)
import Data.Time                            (UTCTime)

setLastModified :: UTCTime -> HandlerT site IO ()
setLastModified = addHeader "Last-Modified" . formatRFC1123

isXHR :: (MonadHandler m) => m Bool
isXHR = do
    req <- waiRequest
    let req_with = lookup "X-Request-With" $ requestHeaders req
    return $ maybe False (== "XMLHTTPRequest") req_with


