{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl where

import Prelude
import qualified Data.Text                  as T
import Yesod
import Data.Text                            (Text)
import Data.MinLen

import Blaze.ByteString.Builder             (Builder)


class JqueryCdnUrl a where
    urlJqueryJsText :: a -> Text

    urlJqueryUiJsText :: a -> Text

    urlJqueryUiCssText :: a -> Text

class JqueryFormCdnUrl a where
    urlJqueryFormJsText :: a -> Text

class BootstrapCdnUrl a where
    urlBootstrapCssText :: a -> Text
    urlBootstrapJsText :: a -> Text

class FontAwesomeCdnUrl a where
    urlFontAwesomeCssText :: a -> Text
    urlFontAwesomeWebFontText :: a -> Text

class HandlebarsCdnUrl a where
    urlHandlebarsJsText ::a -> Text
    urlHandlebarsRuntimeJsText ::a -> Text

class ZeptoCdnUrl a where
    urlZeptoJsText :: a -> Text

class YesodZepto a where
    urlZeptoJs :: a -> Either (Route a) Text

class OnePageScrollCssCdnUrl a where
    urlOnePageScrollCssText :: a -> Text

class OnePageScrollJsJqueryCdnUrl a where
    urlOnePageScrollJsJqueryText :: a -> Text

class YesodOnePageScroll master where
    urlOnePageScrollPureJs :: master -> Either (Route master) Text
    urlOnePageScrollZeptoJs :: master -> Either (Route master) Text
    urlOnePageScrollJqueryJs :: master -> Either (Route master) Text
    urlOnePageScrollCss :: master -> Either (Route master) Text


-- | To offload static files to CDN. See urlRenderOverride
urlRenderOverrideStatic :: (Yesod site, Foldable t, RenderRoute a)
                        => site
                        -> Text
                        -> t Text
                        -> Route a
                        -> Maybe Builder
urlRenderOverrideStatic foundation offload_url safe_exts s = do
    last_p <- Data.MinLen.last <$> (toMinLen ps :: Maybe (MinLen (Succ Zero) [Text]))
    if any (flip T.isSuffixOf last_p) safe_exts
        then
            let ps' = either id id $ cleanPath foundation ps
            in Just $ joinPath foundation offload_url ps' params
        else Nothing

    where
        (ps, params)    = renderRoute s


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

instance JqueryFormCdnUrl BootcssCdn where
    urlJqueryFormJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/jquery.form/3.51/jquery.form.min.js"
            else "//cdn.bootcss.com/jquery.form/3.51/jquery.form.js"

instance BootstrapCdnUrl BootcssCdn where
    urlBootstrapCssText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/bootstrap/3.3.6/css/bootstrap.min.css"
            else "//cdn.bootcss.com/bootstrap/3.3.6/css/bootstrap.css"

    urlBootstrapJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/bootstrap/3.3.6/js/bootstrap.min.js"
            else "//cdn.bootcss.com/bootstrap/3.3.6/js/bootstrap.js"

instance FontAwesomeCdnUrl BootcssCdn where
    urlFontAwesomeCssText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/font-awesome/4.5.0/css/font-awesome.min.css"
            else "//cdn.bootcss.com/font-awesome/4.5.0/css/font-awesome.css"

    urlFontAwesomeWebFontText _ =
        "//cdn.bootcss.com/font-awesome/4.5.0/fonts/fontawesome-webfont.svg"

instance HandlebarsCdnUrl BootcssCdn where
    urlHandlebarsJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/handlebars.js/4.0.5/handlebars.min.js"
            else "//cdn.bootcss.com/handlebars.js/4.0.5/handlebars.min.js"

    urlHandlebarsRuntimeJsText (BootcssCdn min_ver) =
        if min_ver
            then "//cdn.bootcss.com/handlebars.js/4.0.5/handlebars.runtime.min.js"
            else "//cdn.bootcss.com/handlebars.js/4.0.5/handlebars.runtime.js"

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
