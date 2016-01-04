{-# LANGUAGE FlexibleContexts #-}
module Yesod.Helpers.Widget where

import Prelude
import Yesod

import Data.List                            (intersperse)

import Yesod.Helpers.Message


-- | construct page title by merging pieces of texts, separated by MsgPageTitleSep
mergePageTitles :: ( RenderMessage (HandlerSite m) message
                    , RenderMessage (HandlerSite m) YHCommonMessage
                    , MonadWidget m
                    )
                    => [message]
                    -> m ()
mergePageTitles parts = do
    mr <- getMessageRender
    mr2 <- getMessageRender
    setTitleI $ mconcat $ intersperse (mr MsgPageTitleSep) $ map mr2 parts
