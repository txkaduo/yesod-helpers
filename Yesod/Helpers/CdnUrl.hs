{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl where

import Prelude
import Data.Text                            (Text)


data BootcssCdn = BootcssCdn Bool

class JqueryCdnUrl a where
    urlJqueryJsText :: a -> Text

    urlJqueryUiJsText :: a -> Text

    urlJqueryUiCssText :: a -> Text


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
