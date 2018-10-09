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

class JqueryQrcodeCdnUrl a where
    urlJqueryQrcodeJs :: a -> Text

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
  urlJsXlsxFull :: a -> Text

-- | TableExport
class TableExportCdlUrl a where
  urlTableExportJs ::  a -> Text
  urlTableExportCss ::  a -> Text

-- | Moment
class MomentCdnUrl a where
  urlMoment :: a -> Text
  urlMomentWithAllLocale :: a -> Text


class SummerNoteCdnUrl a where
  urlSummerNoteJs :: a -> Text
  urlSummerNoteJsZhCn :: a -> Text
  urlSummerNoteCss :: a -> Text


-- | plupload
class PluploadCdnUrl a where
  urlPluploadFullJs :: a -> Text
  urlPluploadJsZhCn :: a -> Text
  urlPluploadUiJqueryJs :: a -> Text
  urlPluploadUiJqueryCss :: a -> Text


class MoxieCdnUrl a where
  urlMoxieSwf :: a -> Text


class Json3CdnUrl a where
  urlJson3Js :: a -> Text


class NodeUuidCdnUrl a where
  urlNodeUuidJs :: a -> Text


class WeuiCdnUrl a where
  urlWeuiCss :: a -> Text


class JqueryFancyTreeCdnUrl a where
  urlJqueryFancyTreeJs :: a -> Text
  urlJqueryFancyTreeSkinWin8Css :: a -> Text


class JqueryLoadingOverlayCdnUrl a where
  urlJqueryLoadingOverlayJs :: a -> Text


class TooltipsterCdnUrl a where
  urlTooltipsterJs :: a -> Text
  urlTooltipsterCss :: a -> Text

class DropzoneCdnUrl a where
  urlDropzoneJs :: a -> Text
  urlDropzoneCss :: a -> Text

class TimeagoCdnUrl a where
  urlTimeAgoJs :: a -> Text
  urlTimeAgoLocalesJs :: a -> Text


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
            then "https://cdn.bootcss.com/jquery.form/4.2.2/jquery.form.min.js"
            else "https://cdn.bootcss.com/jquery.form/4.2.2/jquery.form.js"

instance JqueryQrcodeCdnUrl BootcssCdn where
  urlJqueryQrcodeJs (BootcssCdn _min_ver) =
    "https://cdn.bootcss.com/jquery.qrcode/1.0/jquery.qrcode.min.js"


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
            then "https://cdn.bootcss.com/handlebars.js/4.0.11/handlebars.min.js"
            else "https://cdn.bootcss.com/handlebars.js/4.0.11/handlebars.js"

    urlHandlebarsRuntimeJsText (BootcssCdn min_ver) =
        if min_ver
            then "https://cdn.bootcss.com/handlebars.js/4.0.11/handlebars.runtime.min.js"
            else "https://cdn.bootcss.com/handlebars.js/4.0.11/handlebars.runtime.js"

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
       then "https://cdn.bootcss.com/video.js/6.10.1/video.min.js"
       else "https://cdn.bootcss.com/video.js/6.10.1/video.js"

  urlVideoJsCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/video.js/6.10.1/video-js.min.css"
       else "https://cdn.bootcss.com/video.js/6.10.1/video-js.css"


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
       then "https://cdn.bootcss.com/FileSaver.js/1.3.8/FileSaver.min.js"
       else "https://cdn.bootcss.com/FileSaver.js/1.3.8/FileSaver.js"


instance JsXlsxCdnUrl BootcssCdn where
  urlJsXlsxCore (BootcssCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then "https://cdn.bootcss.com/xlsx/0.12.13/xlsx.core.min.js"
       else "https://cdn.bootcss.com/xlsx/0.12.13/xlsx.core.min.js"

  urlJsXlsxFull (BootcssCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then "https://cdn.bootcss.com/xlsx/0.12.13/xlsx.full.min.js"
       else "https://cdn.bootcss.com/xlsx/0.12.13/xlsx.full.min.js"


instance TableExportCdlUrl BootcssCdn where
  urlTableExportJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/TableExport/5.0.2/js/tableexport.min.js"
       else "https://cdn.bootcss.com/TableExport/5.0.2/js/tableexport.js"

  urlTableExportCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/TableExport/5.0.2/css/tableexport.min.css"
       else "https://cdn.bootcss.com/TableExport/5.0.2/css/tableexport.css"

instance MomentCdnUrl BootcssCdn where
  urlMoment (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/moment.js/2.22.1/moment.min.js"
       else "https://cdn.bootcss.com/moment.js/2.22.1/moment.js"

  urlMomentWithAllLocale (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/moment.js/2.22.1/moment-with-locales.min.js"
       else "https://cdn.bootcss.com/moment.js/2.22.1/moment-with-locales.js"


instance SummerNoteCdnUrl BootcssCdn where
  urlSummerNoteJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/summernote/0.8.10/summernote.min.js"
       else "https://cdn.bootcss.com/summernote/0.8.10/summernote.js"

  urlSummerNoteJsZhCn (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/summernote/0.8.10/lang/summernote-zh-CN.min.js"
       else "https://cdn.bootcss.com/summernote/0.8.10/lang/summernote-zh-CN.js"

  urlSummerNoteCss (BootcssCdn _min_ver) =
    "https://cdn.bootcss.com/summernote/0.8.10/summernote.css"


instance PluploadCdnUrl BootcssCdn where
  urlPluploadFullJs (BootcssCdn _min_ver) =
    "https://cdn.bootcss.com/plupload/2.3.6/plupload.full.min.js"

  urlPluploadJsZhCn (BootcssCdn _min_ver) =
    "https://cdn.bootcss.com/plupload/2.3.6/i18n/zh_CN.js"

  urlPluploadUiJqueryJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.min.js"
       else "https://cdn.bootcss.com/plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.js"

  urlPluploadUiJqueryCss (BootcssCdn _min_ver) =
    "https://cdn.bootcss.com/plupload/2.3.6/jquery.plupload.queue/css/jquery.plupload.queue.css"


instance MoxieCdnUrl BootcssCdn where
  urlMoxieSwf (BootcssCdn _) =
    "https://cdn.bootcss.com/plupload/2.3.6/Moxie.swf"


instance Json3CdnUrl BootcssCdn where
  urlJson3Js (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/json3/3.3.2/json3.min.js"
       else "https://cdn.bootcss.com/json3/3.3.2/json3.js"


instance NodeUuidCdnUrl BootcssCdn where
  urlNodeUuidJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/node-uuid/1.4.8/uuid.min.js"
       else "https://cdn.bootcss.com/node-uuid/1.4.8/uuid.js"


instance WeuiCdnUrl BootcssCdn where
  urlWeuiCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/weui/1.1.2/style/weui.min.css"
       else "https://cdn.bootcss.com/weui/1.1.2/style/weui.css"


instance JqueryFancyTreeCdnUrl BootcssCdn where
  urlJqueryFancyTreeJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/jquery.fancytree/2.28.1/jquery.fancytree-all.min.js"
       else "https://cdn.bootcss.com/jquery.fancytree/2.28.1/jquery.fancytree-all.js"

  urlJqueryFancyTreeSkinWin8Css (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/jquery.fancytree/2.28.1/skin-win8/ui.fancytree.min.css"
       else "https://cdn.bootcss.com/jquery.fancytree/2.28.1/skin-win8/ui.fancytree.css"


instance JqueryLoadingOverlayCdnUrl BootcssCdn where
  urlJqueryLoadingOverlayJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/jquery-loading-overlay/2.1.3/loadingoverlay.min.js"
       else "https://cdn.bootcss.com/jquery-loading-overlay/2.1.3/loadingoverlay.js"

instance TooltipsterCdnUrl BootcssCdn where
  urlTooltipsterJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/tooltipster/3.3.0/js/jquery.tooltipster.min.js"
       else "https://cdn.bootcss.com/tooltipster/3.3.0/js/jquery.tooltipster.js"

  urlTooltipsterCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/tooltipster/3.3.0/css/tooltipster.min.css"
       else "https://cdn.bootcss.com/tooltipster/3.3.0/css/tooltipster.css"

instance DropzoneCdnUrl BootcssCdn where
  urlDropzoneJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/dropzone/5.5.1/min/dropzone.min.js"
       else "https://cdn.bootcss.com/dropzone/5.5.1/dropzone.js"

  urlDropzoneCss (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/dropzone/5.5.1/min/dropzone.min.css"
       else "https://cdn.bootcss.com/dropzone/5.5.1/dropzone.css"

instance TimeagoCdnUrl BootcssCdn where
  urlTimeAgoJs (BootcssCdn min_ver) =
    if min_ver
       then "https://cdn.bootcss.com/timeago.js/3.0.2/timeago.min.js"
       else "https://cdn.bootcss.com/timeago.js/3.0.2/timeago.js"

  urlTimeAgoLocalesJs (BootcssCdn min_ver) =
    -- only min version available
    if min_ver
       then "https://cdn.bootcss.com/timeago.js/3.0.2/timeago.locales.min.js"
       else "https://cdn.bootcss.com/timeago.js/3.0.2/timeago.locales.min.js"


data StaticFileCdn = StaticFileCdn Bool

instance JqueryCdnUrl StaticFileCdn where
    urlJqueryJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/jquery/1.12.4/jquery.min.js"
            else "https://cdn.staticfile.org/jquery/1.12.4/jquery.js"

    urlJqueryUiJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/jqueryui/1.12.1/jquery-ui.min.js"
            else "https://cdn.staticfile.org/jqueryui/1.12.1/jquery-ui.js"

    urlJqueryUiCssText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/jqueryui/1.12.1/jquery-ui.min.css"
            else "https://cdn.staticfile.org/jqueryui/1.12.1/jquery-ui.css"


instance JqueryFormCdnUrl StaticFileCdn where
    urlJqueryFormJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/jquery.form/4.2.2/jquery.form.min.js"
            else "https://cdn.staticfile.org/jquery.form/4.2.2/jquery.form.js"


instance JqueryQrcodeCdnUrl StaticFileCdn where
  urlJqueryQrcodeJs (StaticFileCdn _min_ver) =
    "https://cdn.staticfile.org/jquery.qrcode/1.0/jquery.qrcode.min.js"


instance BootstrapCdnUrl StaticFileCdn where
    urlBootstrapCssText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/twitter-bootstrap/3.3.7/css/bootstrap.min.css"
            else "https://cdn.staticfile.org/twitter-bootstrap/3.3.7/css/bootstrap.css"

    urlBootstrapJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/twitter-bootstrap/3.3.7/js/bootstrap.min.js"
            else "https://cdn.staticfile.org/twitter-bootstrap/3.3.7/js/bootstrap.js"


instance FontAwesomeCdnUrl StaticFileCdn where
    urlFontAwesomeCssText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/font-awesome/4.7.0/css/font-awesome.min.css"
            else "https://cdn.staticfile.org/font-awesome/4.7.0/css/font-awesome.css"

    urlFontAwesomeWebFontText _ =
        "https://cdn.staticfile.org/font-awesome/4.7.0/fonts/fontawesome-webfont.svg"


instance HandlebarsCdnUrl StaticFileCdn where
    urlHandlebarsJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/handlebars.js/4.0.12/handlebars.min.js"
            else "https://cdn.staticfile.org/handlebars.js/4.0.12/handlebars.js"

    urlHandlebarsRuntimeJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/handlebars.js/4.0.12/handlebars.runtime.min.js"
            else "https://cdn.staticfile.org/handlebars.js/4.0.12/handlebars.runtime.js"


instance ZeptoCdnUrl StaticFileCdn where
    urlZeptoJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/zepto/1.1.7/zepto.min.js"
            else "https://cdn.staticfile.org/zepto/1.1.7/zepto.js"

instance OnePageScrollCssCdnUrl StaticFileCdn where
    urlOnePageScrollCssText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/onepage-scroll/1.3.1/onepage-scroll.min.css"
            else "https://cdn.staticfile.org/onepage-scroll/1.3.1/onepage-scroll.css"

instance OnePageScrollJsJqueryCdnUrl StaticFileCdn where
    urlOnePageScrollJsJqueryText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/onepage-scroll/1.3.1/jquery.onepage-scroll.min.js"
            else "https://cdn.staticfile.org/onepage-scroll/1.3.1/jquery.onepage-scroll.js"

instance JsCookieCdnUrl StaticFileCdn where
    urlJsCookieJsText (StaticFileCdn min_ver) =
        if min_ver
            then "https://cdn.staticfile.org/js-cookie/latest/js.cookie.min.js"
            else "https://cdn.staticfile.org/js-cookie/latest/js.cookie.js"

instance TypeaheadCdnUrl StaticFileCdn where
  urlTypeaheadBundleJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/typeahead.js/0.11.1/typeahead.bundle.min.js"
       else "https://cdn.staticfile.org/typeahead.js/0.11.1/typeahead.bundle.js"


instance VideoJsCdnUrl StaticFileCdn where
  urlVideoJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/video.js/6.12.1/video.min.js"
       else "https://cdn.staticfile.org/video.js/6.12.1/video.js"

  urlVideoJsCss (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/video.js/6.12.1/video-js.min.css"
       else "https://cdn.staticfile.org/video.js/6.12.1/video-js.css"


instance VideoJsOverlayCdnUrl StaticFileCdn where
  urlVideoJsOverlay (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/videojs-overlay/1.1.4/videojs-overlay.min.js"
       else "https://cdn.staticfile.org/videojs-overlay/1.1.4/videojs-overlay.js"

  urlVideoJsOverlayCss (StaticFileCdn _min_ver) =
    "https://cdn.staticfile.org/videojs-overlay/1.1.4/videojs-overlay.css"


instance ClipboardJsCdnUrl StaticFileCdn where
  urlClipboardJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/clipboard.js/1.7.1/clipboard.min.js"
       else "https://cdn.staticfile.org/clipboard.js/1.7.1/clipboard.js"


instance FileSaverCdnUrl StaticFileCdn where
  urlFileSaverJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/FileSaver.js/1.3.8/FileSaver.min.js"
       else "https://cdn.staticfile.org/FileSaver.js/1.3.8/FileSaver.js"


instance JsXlsxCdnUrl StaticFileCdn where
  urlJsXlsxCore (StaticFileCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then "https://cdn.staticfile.org/xlsx/0.14.0/xlsx.core.min.js"
       else "https://cdn.staticfile.org/xlsx/0.14.0/xlsx.core.min.js"

  urlJsXlsxFull (StaticFileCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then "https://cdn.staticfile.org/xlsx/0.14.0/xlsx.full.min.js"
       else "https://cdn.staticfile.org/xlsx/0.14.0/xlsx.full.min.js"


instance TableExportCdlUrl StaticFileCdn where
  urlTableExportJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/TableExport/5.0.2/js/tableexport.min.js"
       else "https://cdn.staticfile.org/TableExport/5.0.2/js/tableexport.js"

  urlTableExportCss (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/TableExport/5.0.2/css/tableexport.min.css"
       else "https://cdn.staticfile.org/TableExport/5.0.2/css/tableexport.css"


instance MomentCdnUrl StaticFileCdn where
  urlMoment (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/moment.js/2.22.2/moment.min.js"
       else "https://cdn.staticfile.org/moment.js/2.22.2/moment.js"

  urlMomentWithAllLocale (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/moment.js/2.22.2/moment-with-locales.min.js"
       else "https://cdn.staticfile.org/moment.js/2.22.2/moment-with-locales.js"


instance SummerNoteCdnUrl StaticFileCdn where
  urlSummerNoteJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/summernote/0.8.10/summernote.min.js"
       else "https://cdn.staticfile.org/summernote/0.8.10/summernote.js"

  urlSummerNoteJsZhCn (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/summernote/0.8.10/lang/summernote-zh-CN.min.js"
       else "https://cdn.staticfile.org/summernote/0.8.10/lang/summernote-zh-CN.js"

  urlSummerNoteCss (StaticFileCdn _min_ver) =
    "https://cdn.staticfile.org/summernote/0.8.10/summernote.css"


instance PluploadCdnUrl StaticFileCdn where
  urlPluploadFullJs (StaticFileCdn _min_ver) =
    "https://cdn.staticfile.org/plupload/2.3.6/plupload.full.min.js"

  urlPluploadJsZhCn (StaticFileCdn _min_ver) =
    "https://cdn.staticfile.org/plupload/2.3.6/i18n/zh_CN.js"

  urlPluploadUiJqueryJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.min.js"
       else "https://cdn.staticfile.org/plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.js"

  urlPluploadUiJqueryCss (StaticFileCdn _min_ver) =
    "https://cdn.staticfile.org/plupload/2.3.6/jquery.plupload.queue/css/jquery.plupload.queue.css"


instance MoxieCdnUrl StaticFileCdn where
  urlMoxieSwf (StaticFileCdn _) =
    "https://cdn.staticfile.org/plupload/2.3.6/Moxie.swf"


instance Json3CdnUrl StaticFileCdn where
  urlJson3Js (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/json3/3.3.2/json3.min.js"
       else "https://cdn.staticfile.org/json3/3.3.2/json3.js"


instance NodeUuidCdnUrl StaticFileCdn where
  urlNodeUuidJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/node-uuid/1.4.8/uuid.min.js"
       else "https://cdn.staticfile.org/node-uuid/1.4.8/uuid.js"


instance WeuiCdnUrl StaticFileCdn where
  urlWeuiCss (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/weui/1.1.3/style/weui.min.css"
       else "https://cdn.staticfile.org/weui/1.1.3/style/weui.css"


instance JqueryFancyTreeCdnUrl StaticFileCdn where
  urlJqueryFancyTreeJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/jquery.fancytree/2.30.0/jquery.fancytree-all.min.js"
       else "https://cdn.staticfile.org/jquery.fancytree/2.30.0/jquery.fancytree-all.js"

  urlJqueryFancyTreeSkinWin8Css (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/jquery.fancytree/2.30.0/skin-win8/ui.fancytree.min.css"
       else "https://cdn.staticfile.org/jquery.fancytree/2.30.0/skin-win8/ui.fancytree.css"


instance JqueryLoadingOverlayCdnUrl StaticFileCdn where
  urlJqueryLoadingOverlayJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/jquery-loading-overlay/2.1.6/loadingoverlay.min.js"
       else "https://cdn.staticfile.org/jquery-loading-overlay/2.1.6/loadingoverlay.js"


instance TooltipsterCdnUrl StaticFileCdn where
  urlTooltipsterJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/tooltipster/3.3.0/js/jquery.tooltipster.min.js"
       else "https://cdn.staticfile.org/tooltipster/3.3.0/js/jquery.tooltipster.js"

  urlTooltipsterCss (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/tooltipster/3.3.0/css/tooltipster.min.css"
       else "https://cdn.staticfile.org/tooltipster/3.3.0/css/tooltipster.css"

instance DropzoneCdnUrl StaticFileCdn where
  urlDropzoneJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.com/dropzone/5.5.1/min/dropzone.min.js"
       else "https://cdn.staticfile.com/dropzone/5.5.1/dropzone.js"

  urlDropzoneCss (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.com/dropzone/5.5.1/min/dropzone.min.css"
       else "https://cdn.staticfile.com/dropzone/5.5.1/dropzone.css"

instance TimeagoCdnUrl StaticFileCdn where
  urlTimeAgoJs (StaticFileCdn min_ver) =
    if min_ver
       then "https://cdn.staticfile.org/timeago.js/3.0.2/timeago.min.js"
       else "https://cdn.staticfile.org/timeago.js/3.0.2/timeago.js"

  urlTimeAgoLocalesJs (StaticFileCdn min_ver) =
    -- only min version available
    if min_ver
       then "https://cdn.staticfile.org/timeago.js/3.0.2/timeago.locales.min.js"
       else "https://cdn.staticfile.org/timeago.js/3.0.2/timeago.locales.min.js"



type CoreStaticCdnUrl a = (JqueryCdnUrl a, BootstrapCdnUrl a, FontAwesomeCdnUrl a, ZeptoCdnUrl a)

type FullStaticCdnUrl a = ( CoreStaticCdnUrl a
                          , JqueryFormCdnUrl a
                          , JqueryQrcodeCdnUrl a
                          , HandlebarsCdnUrl a
                          , OnePageScrollCssCdnUrl a
                          , OnePageScrollJsJqueryCdnUrl a
                          , JsCookieCdnUrl a
                          , TypeaheadCdnUrl a
                          , VideoJsCdnUrl a
                          , VideoJsOverlayCdnUrl a
                          , ClipboardJsCdnUrl a
                          , FileSaverCdnUrl a
                          , JsXlsxCdnUrl a
                          , TableExportCdlUrl a
                          , MomentCdnUrl a
                          , SummerNoteCdnUrl a
                          , PluploadCdnUrl a
                          , MoxieCdnUrl a
                          , Json3CdnUrl a
                          , NodeUuidCdnUrl a
                          , WeuiCdnUrl a
                          , JqueryFancyTreeCdnUrl a
                          , JqueryLoadingOverlayCdnUrl a
                          , TooltipsterCdnUrl a
                          , DropzoneCdnUrl a
                          , TimeagoCdnUrl a
                          )


data SomeFullStaticCdnUrl = forall a. FullStaticCdnUrl a => SomeFullStaticCdnUrl a

instance JqueryCdnUrl SomeFullStaticCdnUrl where
  urlJqueryJsText (SomeFullStaticCdnUrl x)    = urlJqueryJsText x
  urlJqueryUiJsText (SomeFullStaticCdnUrl x)  = urlJqueryUiJsText x
  urlJqueryUiCssText (SomeFullStaticCdnUrl x) = urlJqueryUiCssText x

instance JqueryFormCdnUrl SomeFullStaticCdnUrl where
  urlJqueryFormJsText (SomeFullStaticCdnUrl x) = urlJqueryFormJsText x

instance JqueryQrcodeCdnUrl SomeFullStaticCdnUrl where
  urlJqueryQrcodeJs (SomeFullStaticCdnUrl x) = urlJqueryQrcodeJs x

instance BootstrapCdnUrl SomeFullStaticCdnUrl where
  urlBootstrapCssText (SomeFullStaticCdnUrl x) = urlBootstrapCssText x
  urlBootstrapJsText (SomeFullStaticCdnUrl x) = urlBootstrapJsText x

instance FontAwesomeCdnUrl SomeFullStaticCdnUrl where
  urlFontAwesomeCssText (SomeFullStaticCdnUrl x) = urlFontAwesomeCssText x
  urlFontAwesomeWebFontText (SomeFullStaticCdnUrl x) = urlFontAwesomeWebFontText x

instance HandlebarsCdnUrl SomeFullStaticCdnUrl where
  urlHandlebarsJsText (SomeFullStaticCdnUrl x) = urlHandlebarsJsText x
  urlHandlebarsRuntimeJsText (SomeFullStaticCdnUrl x) = urlHandlebarsRuntimeJsText x

instance ZeptoCdnUrl SomeFullStaticCdnUrl where
  urlZeptoJsText (SomeFullStaticCdnUrl x) = urlZeptoJsText x

instance OnePageScrollCssCdnUrl SomeFullStaticCdnUrl where
  urlOnePageScrollCssText (SomeFullStaticCdnUrl x) = urlOnePageScrollCssText x

instance OnePageScrollJsJqueryCdnUrl SomeFullStaticCdnUrl where
  urlOnePageScrollJsJqueryText (SomeFullStaticCdnUrl x) = urlOnePageScrollJsJqueryText x

instance JsCookieCdnUrl SomeFullStaticCdnUrl where
  urlJsCookieJsText (SomeFullStaticCdnUrl x) = urlJsCookieJsText x

instance TypeaheadCdnUrl SomeFullStaticCdnUrl where
  urlTypeaheadBundleJs (SomeFullStaticCdnUrl x) = urlTypeaheadBundleJs x

instance VideoJsCdnUrl SomeFullStaticCdnUrl where
  urlVideoJs (SomeFullStaticCdnUrl x) = urlVideoJs x
  urlVideoJsCss (SomeFullStaticCdnUrl x) = urlVideoJsCss x

instance VideoJsOverlayCdnUrl SomeFullStaticCdnUrl where
  urlVideoJsOverlay (SomeFullStaticCdnUrl x) = urlVideoJsOverlay x
  urlVideoJsOverlayCss (SomeFullStaticCdnUrl x) = urlVideoJsOverlayCss x

instance ClipboardJsCdnUrl SomeFullStaticCdnUrl where
  urlClipboardJs (SomeFullStaticCdnUrl x) = urlClipboardJs x

instance FileSaverCdnUrl SomeFullStaticCdnUrl where
  urlFileSaverJs (SomeFullStaticCdnUrl x) = urlFileSaverJs x

instance JsXlsxCdnUrl SomeFullStaticCdnUrl where
  urlJsXlsxCore (SomeFullStaticCdnUrl x) = urlJsXlsxCore x
  urlJsXlsxFull (SomeFullStaticCdnUrl x) = urlJsXlsxFull x

instance TableExportCdlUrl SomeFullStaticCdnUrl where
  urlTableExportJs (SomeFullStaticCdnUrl x) = urlTableExportJs x
  urlTableExportCss (SomeFullStaticCdnUrl x) = urlTableExportCss x

instance MomentCdnUrl SomeFullStaticCdnUrl where
  urlMoment (SomeFullStaticCdnUrl x) = urlMoment x
  urlMomentWithAllLocale (SomeFullStaticCdnUrl x) = urlMomentWithAllLocale x

instance SummerNoteCdnUrl SomeFullStaticCdnUrl where
  urlSummerNoteJs (SomeFullStaticCdnUrl x) = urlSummerNoteJs x
  urlSummerNoteJsZhCn (SomeFullStaticCdnUrl x) = urlSummerNoteJsZhCn x
  urlSummerNoteCss (SomeFullStaticCdnUrl x) = urlSummerNoteCss x

instance PluploadCdnUrl SomeFullStaticCdnUrl where
  urlPluploadFullJs (SomeFullStaticCdnUrl x) = urlPluploadFullJs x
  urlPluploadJsZhCn (SomeFullStaticCdnUrl x) = urlPluploadJsZhCn x
  urlPluploadUiJqueryJs (SomeFullStaticCdnUrl x) = urlPluploadUiJqueryJs x
  urlPluploadUiJqueryCss (SomeFullStaticCdnUrl x) = urlPluploadUiJqueryCss x

instance MoxieCdnUrl SomeFullStaticCdnUrl where
  urlMoxieSwf (SomeFullStaticCdnUrl x) = urlMoxieSwf x

instance Json3CdnUrl SomeFullStaticCdnUrl where
  urlJson3Js (SomeFullStaticCdnUrl x) = urlJson3Js x

instance NodeUuidCdnUrl SomeFullStaticCdnUrl where
  urlNodeUuidJs (SomeFullStaticCdnUrl x) = urlNodeUuidJs x

instance WeuiCdnUrl SomeFullStaticCdnUrl where
  urlWeuiCss (SomeFullStaticCdnUrl x) = urlWeuiCss x

instance JqueryFancyTreeCdnUrl SomeFullStaticCdnUrl where
  urlJqueryFancyTreeJs (SomeFullStaticCdnUrl x) = urlJqueryFancyTreeJs x
  urlJqueryFancyTreeSkinWin8Css (SomeFullStaticCdnUrl x) = urlJqueryFancyTreeSkinWin8Css x

instance JqueryLoadingOverlayCdnUrl SomeFullStaticCdnUrl where
  urlJqueryLoadingOverlayJs (SomeFullStaticCdnUrl x) = urlJqueryLoadingOverlayJs x

instance TooltipsterCdnUrl SomeFullStaticCdnUrl where
  urlTooltipsterJs (SomeFullStaticCdnUrl x) = urlTooltipsterJs x
  urlTooltipsterCss (SomeFullStaticCdnUrl x) = urlTooltipsterCss x

instance DropzoneCdnUrl SomeFullStaticCdnUrl where
  urlDropzoneJs (SomeFullStaticCdnUrl x) = urlDropzoneJs x
  urlDropzoneCss (SomeFullStaticCdnUrl x) = urlDropzoneCss x

instance TimeagoCdnUrl SomeFullStaticCdnUrl where
  urlTimeAgoJs (SomeFullStaticCdnUrl x) = urlTimeAgoJs x
  urlTimeAgoLocalesJs (SomeFullStaticCdnUrl x) = urlTimeAgoLocalesJs x

