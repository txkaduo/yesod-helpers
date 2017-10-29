{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl where

import Prelude
import qualified Data.Text                  as T
import Yesod
import Data.Text                            (Text)
import Data.NonNull

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

-- | see: https://github.com/js-cookie/js-cookie
class JsCookieCdnUrl a where
    urlJsCookieJsText :: a -> Text

class YesodJsCookie a where
    urlJsCookieJs :: a -> Either (Route a) Text


-- | Typeahead library from twitter
class TypeaheadCdnUrl a where
  urlTypeaheadBundleJs :: a -> Text


-- | For video.js
class VideoJsCdnUrl a where
  urlVideoJs :: a -> Text
  urlVideoJsCss :: a -> Text

-- | For video.js plugin: videojs-overlay
class VideoJsOverlayCdnUrl a where
  urlVideoJsOverlay :: a -> Text
  urlVideoJsOverlayCss :: a -> Text


-- | clipboard.js
class ClipboardJsCdnUrl a where
  urlClipboardJs :: a -> Text


-- | FileSaver.js
class FileSaverCdnUrl a where
  urlFileSaverJs :: a -> Text


class JsXlsxCdnUrl a where
  urlJsXlsxCore :: a -> Text

-- | TableExport
class TableExportCdlUrl a where
  urlTableExportJs ::  a -> Text
  urlTableExportCss ::  a -> Text


-- | To offload static files to CDN. See urlRenderOverride
urlRenderOverrideStatic :: (Yesod site, Foldable t, RenderRoute a)
                        => site
                        -> Text
                        -> t Text
                        -> Route a
                        -> Maybe Builder
urlRenderOverrideStatic foundation offload_url safe_exts s = do
    last_p <- Data.NonNull.last <$> fromNullable ps
    if null safe_exts || any (flip T.isSuffixOf last_p) safe_exts
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
            then "https://cdn.bootcss.com/jquery/1.12.4/jquery.min.js"
            else "https://cdn.bootcss.com/jquery/1.12.4/jquery.js"

    urlJqueryUiJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/jqueryui/1.12.1/jquery-ui.min.js"
            else "https://cdn.bootcss.com/jqueryui/1.12.1/jquery-ui.js"

    urlJqueryUiCssText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/jqueryui/1.12.1/jquery-ui.min.css"
            else "https://cdn.bootcss.com/jqueryui/1.12.1/jquery-ui.css"

instance JqueryFormCdnUrl BootcssCdn where
    urlJqueryFormJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/jquery.form/4.2.1/jquery.form.min.js"
            else "https://cdn.bootcss.com/jquery.form/4.2.1/jquery.form.js"

instance BootstrapCdnUrl BootcssCdn where
    urlBootstrapCssText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/bootstrap/3.3.7/css/bootstrap.min.css"
            else "https://cdn.bootcss.com/bootstrap/3.3.7/css/bootstrap.css"

    urlBootstrapJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/bootstrap/3.3.7/js/bootstrap.min.js"
            else "https://cdn.bootcss.com/bootstrap/3.3.7/js/bootstrap.js"

instance FontAwesomeCdnUrl BootcssCdn where
    urlFontAwesomeCssText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/font-awesome/4.7.0/css/font-awesome.min.css"
            else "https://cdn.bootcss.com/font-awesome/4.7.0/css/font-awesome.css"

    urlFontAwesomeWebFontText _ =
        "https://cdn.bootcss.com/font-awesome/4.7.0/fonts/fontawesome-webfont.svg"

instance HandlebarsCdnUrl BootcssCdn where
    urlHandlebarsJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/handlebars.js/4.0.8/handlebars.min.js"
            else "https://cdn.bootcss.com/handlebars.js/4.0.8/handlebars.min.js"

    urlHandlebarsRuntimeJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/handlebars.js/4.0.8/handlebars.runtime.min.js"
            else "https://cdn.bootcss.com/handlebars.js/4.0.8/handlebars.runtime.js"

instance ZeptoCdnUrl BootcssCdn where
    urlZeptoJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/zepto/1.1.7/zepto.min.js"
            else "https://cdn.bootcss.com/zepto/1.1.7/zepto.js"

instance OnePageScrollCssCdnUrl BootcssCdn where
    urlOnePageScrollCssText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/onepage-scroll/1.3.1/onepage-scroll.min.css"
            else "https://cdn.bootcss.com/onepage-scroll/1.3.1/onepage-scroll.css"

instance OnePageScrollJsJqueryCdnUrl BootcssCdn where
    urlOnePageScrollJsJqueryText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/onepage-scroll/1.3.1/jquery.onepage-scroll.min.js"
            else "https://cdn.bootcss.com/onepage-scroll/1.3.1/jquery.onepage-scroll.js"

instance JsCookieCdnUrl BootcssCdn where
    urlJsCookieJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/js-cookie/2.1.4/js.cookie.min.js"
            else "https://cdn.bootcss.com/js-cookie/2.1.4/js.cookie.js"

instance TypeaheadCdnUrl BootcssCdn where
  urlTypeaheadBundleJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/typeahead.js/0.11.1/typeahead.bundle.min.js"
       else "https://cdn.bootcss.com/typeahead.js/0.11.1/typeahead.bundle.js"


instance VideoJsCdnUrl BootcssCdn where
  urlVideoJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/video.js/6.2.6/video.min.js"
       else "https://cdn.bootcss.com/video.js/6.2.6/video.js"

  urlVideoJsCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/video.js/6.2.6/video-js.min.css"
       else "https://cdn.bootcss.com/video.js/6.2.6/video-js.css"


instance VideoJsOverlayCdnUrl BootcssCdn where
  urlVideoJsOverlay (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/videojs-overlay/1.1.4/videojs-overlay.min.js"
       else "https://cdn.bootcss.com/videojs-overlay/1.1.4/videojs-overlay.js"

  urlVideoJsOverlayCss (BootcssCdn _min_ver) =
    "https://cdn.bootcss.com/videojs-overlay/1.1.4/videojs-overlay.css"


instance ClipboardJsCdnUrl BootcssCdn where
  urlClipboardJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/clipboard.js/1.7.1/clipboard.min.js"
       else "https://cdn.bootcss.com/clipboard.js/1.7.1/clipboard.js"


instance FileSaverCdnUrl BootcssCdn where
  urlFileSaverJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/FileSaver.js/1.3.3/FileSaver.min.js"
       else "https://cdn.bootcss.com/FileSaver.js/1.3.3/FileSaver.js"


instance JsXlsxCdnUrl BootcssCdn where
  urlJsXlsxCore (BootcssCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then "https://cdn.bootcss.com/xlsx/0.11.5/xlsx.core.min.js"
       else "https://cdn.bootcss.com/xlsx/0.11.5/xlsx.core.min.js"


instance TableExportCdlUrl BootcssCdn where
  urlTableExportJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/TableExport/4.0.11/js/tableexport.min.js"
       else "https://cdn.bootcss.com/TableExport/4.0.11/js/tableexport.js"

  urlTableExportCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/TableExport/4.0.11/css/tableexport.min.css"
       else "https://cdn.bootcss.com/TableExport/4.0.11/css/tableexport.css"
