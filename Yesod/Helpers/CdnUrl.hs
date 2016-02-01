{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl where

import Prelude
import Data.Text                            (Text)


class JqueryCdnUrl a where
    urlJqueryJsText :: a -> Text

    urlJqueryUiJsText :: a -> Text

    urlJqueryUiCssText :: a -> Text


class ZeptoCdnUrl a where
    urlZeptoJsText :: a -> Text

class OnePageScrollCssCdnUrl a where
    urlOnePageScrollCssText :: a -> Text

class OnePageScrollJsJqueryCdnUrl a where
    urlOnePageScrollJsJqueryText :: a -> Text


data BootcssCdn = BootcssCdn Bool

instance JqueryCdnUrl BootcssCdn where
    urlJqueryJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/jquery/1.11.3/jquery.min.js"
            else "//cdn.bootcss.com/jquery/1.11.3/jquery.js"

    urlJqueryUiJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/jqueryui/1.11.4/jquery-ui.min.js"
            else "//cdn.bootcss.com/jqueryui/1.11.4/jquery-ui.js"

    urlJqueryUiCssText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/jqueryui/1.11.4/jquery-ui.min.css"
            else "//cdn.bootcss.com/jqueryui/1.11.4/jquery-ui.css"

instance ZeptoCdnUrl BootcssCdn where
    urlZeptoJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/zepto/1.1.6/zepto.min.js"
            else "//cdn.bootcss.com/zepto/1.1.6/zepto.js"

instance OnePageScrollCssCdnUrl BootcssCdn where
    urlOnePageScrollCssText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/onepage-scroll/1.3.1/onepage-scroll.min.css"
            else "//cdn.bootcss.com/onepage-scroll/1.3.1/onepage-scroll.css"

instance OnePageScrollJsJqueryCdnUrl BootcssCdn where
    urlOnePageScrollJsJqueryText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/onepage-scroll/1.3.1/jquery.onepage-scroll.min.js"
            else "//cdn.bootcss.com/onepage-scroll/1.3.1/jquery.onepage-scroll.js"
