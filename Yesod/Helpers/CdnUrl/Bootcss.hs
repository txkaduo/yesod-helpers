{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl.Bootcss
  ( BootcssCdn(..) )
  where

import Yesod.Helpers.CdnUrl.Class
import Data.Text (Text)

data BootcssCdn = BootcssCdn Bool

bootcssCdnUrl :: Text -> Text
bootcssCdnUrl = (<>) "https://cdn.bootcss.com/"

instance JqueryCdnUrl BootcssCdn where
    urlJqueryJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "jquery/1.12.4/jquery.min.js"
            else bootcssCdnUrl "jquery/1.12.4/jquery.js"

    urlJqueryUiJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "jqueryui/1.12.1/jquery-ui.min.js"
            else bootcssCdnUrl "jqueryui/1.12.1/jquery-ui.js"

    urlJqueryUiCssText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "jqueryui/1.12.1/jquery-ui.min.css"
            else bootcssCdnUrl "jqueryui/1.12.1/jquery-ui.css"

instance JqueryFormCdnUrl BootcssCdn where
    urlJqueryFormJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "jquery.form/4.2.2/jquery.form.min.js"
            else bootcssCdnUrl "jquery.form/4.2.2/jquery.form.js"

instance JqueryQrcodeCdnUrl BootcssCdn where
  urlJqueryQrcodeJs (BootcssCdn _min_ver) =
    bootcssCdnUrl "jquery.qrcode/1.0/jquery.qrcode.min.js"


instance BootstrapCdnUrl BootcssCdn where
    urlBootstrapCssText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "bootstrap/3.3.7/css/bootstrap.min.css"
            else bootcssCdnUrl "bootstrap/3.3.7/css/bootstrap.css"

    urlBootstrapJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "bootstrap/3.3.7/js/bootstrap.min.js"
            else bootcssCdnUrl "bootstrap/3.3.7/js/bootstrap.js"

instance FontAwesomeCdnUrl BootcssCdn where
    urlFontAwesomeCssText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "font-awesome/4.7.0/css/font-awesome.min.css"
            else bootcssCdnUrl "font-awesome/4.7.0/css/font-awesome.css"

    urlFontAwesomeWebFontText _ =
        bootcssCdnUrl "font-awesome/4.7.0/fonts/fontawesome-webfont.svg"

instance HandlebarsCdnUrl BootcssCdn where
    urlHandlebarsJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "handlebars.js/4.0.11/handlebars.min.js"
            else bootcssCdnUrl "handlebars.js/4.0.11/handlebars.js"

    urlHandlebarsRuntimeJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "handlebars.js/4.0.11/handlebars.runtime.min.js"
            else bootcssCdnUrl "handlebars.js/4.0.11/handlebars.runtime.js"

instance ZeptoCdnUrl BootcssCdn where
    urlZeptoJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "zepto/1.1.7/zepto.min.js"
            else bootcssCdnUrl "zepto/1.1.7/zepto.js"

instance OnePageScrollCssCdnUrl BootcssCdn where
    urlOnePageScrollCssText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "onepage-scroll/1.3.1/onepage-scroll.min.css"
            else bootcssCdnUrl "onepage-scroll/1.3.1/onepage-scroll.css"

instance OnePageScrollJsJqueryCdnUrl BootcssCdn where
    urlOnePageScrollJsJqueryText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "onepage-scroll/1.3.1/jquery.onepage-scroll.min.js"
            else bootcssCdnUrl "onepage-scroll/1.3.1/jquery.onepage-scroll.js"

instance JsCookieCdnUrl BootcssCdn where
    urlJsCookieJsText (BootcssCdn min_ver) =
        if min_ver
            then bootcssCdnUrl "js-cookie/2.1.4/js.cookie.min.js"
            else bootcssCdnUrl "js-cookie/2.1.4/js.cookie.js"

instance TypeaheadCdnUrl BootcssCdn where
  urlTypeaheadBundleJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "typeahead.js/0.11.1/typeahead.bundle.min.js"
       else bootcssCdnUrl "typeahead.js/0.11.1/typeahead.bundle.js"


instance VideoJsCdnUrl BootcssCdn where
  urlVideoJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "video.js/6.10.1/video.min.js"
       else bootcssCdnUrl "video.js/6.10.1/video.js"

  urlVideoJsCss (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "video.js/6.10.1/video-js.min.css"
       else bootcssCdnUrl "video.js/6.10.1/video-js.css"


instance VideoJsOverlayCdnUrl BootcssCdn where
  urlVideoJsOverlay (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "videojs-overlay/1.1.4/videojs-overlay.min.js"
       else bootcssCdnUrl "videojs-overlay/1.1.4/videojs-overlay.js"

  urlVideoJsOverlayCss (BootcssCdn _min_ver) =
    bootcssCdnUrl "videojs-overlay/1.1.4/videojs-overlay.css"


instance ClipboardJsCdnUrl BootcssCdn where
  urlClipboardJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "clipboard.js/1.7.1/clipboard.min.js"
       else bootcssCdnUrl "clipboard.js/1.7.1/clipboard.js"


instance FileSaverCdnUrl BootcssCdn where
  urlFileSaverJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "FileSaver.js/1.3.8/FileSaver.min.js"
       else bootcssCdnUrl "FileSaver.js/1.3.8/FileSaver.js"


instance JsXlsxCdnUrl BootcssCdn where
  urlJsXlsxCore (BootcssCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then bootcssCdnUrl "xlsx/0.12.13/xlsx.core.min.js"
       else bootcssCdnUrl "xlsx/0.12.13/xlsx.core.min.js"

  urlJsXlsxFull (BootcssCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then bootcssCdnUrl "xlsx/0.12.13/xlsx.full.min.js"
       else bootcssCdnUrl "xlsx/0.12.13/xlsx.full.min.js"


instance TableExportCdlUrl BootcssCdn where
  urlTableExportJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "TableExport/5.0.2/js/tableexport.min.js"
       else bootcssCdnUrl "TableExport/5.0.2/js/tableexport.js"

  urlTableExportCss (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "TableExport/5.0.2/css/tableexport.min.css"
       else bootcssCdnUrl "TableExport/5.0.2/css/tableexport.css"

instance MomentCdnUrl BootcssCdn where
  urlMoment (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "moment.js/2.22.1/moment.min.js"
       else bootcssCdnUrl "moment.js/2.22.1/moment.js"

  urlMomentWithAllLocale (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "moment.js/2.22.1/moment-with-locales.min.js"
       else bootcssCdnUrl "moment.js/2.22.1/moment-with-locales.js"


instance SummerNoteCdnUrl BootcssCdn where
  urlSummerNoteJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "summernote/0.8.10/summernote.min.js"
       else bootcssCdnUrl "summernote/0.8.10/summernote.js"

  urlSummerNoteJsZhCn (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "summernote/0.8.10/lang/summernote-zh-CN.min.js"
       else bootcssCdnUrl "summernote/0.8.10/lang/summernote-zh-CN.js"

  urlSummerNoteCss (BootcssCdn _min_ver) =
    bootcssCdnUrl "summernote/0.8.10/summernote.css"


instance PluploadCdnUrl BootcssCdn where
  urlPluploadFullJs (BootcssCdn _min_ver) =
    bootcssCdnUrl "plupload/2.3.6/plupload.full.min.js"

  urlPluploadJsZhCn (BootcssCdn _min_ver) =
    bootcssCdnUrl "plupload/2.3.6/i18n/zh_CN.js"

  urlPluploadUiJqueryJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.min.js"
       else bootcssCdnUrl "plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.js"

  urlPluploadUiJqueryCss (BootcssCdn _min_ver) =
    bootcssCdnUrl "plupload/2.3.6/jquery.plupload.queue/css/jquery.plupload.queue.css"


instance MoxieCdnUrl BootcssCdn where
  urlMoxieSwf (BootcssCdn _) =
    bootcssCdnUrl "plupload/2.3.6/Moxie.swf"


instance Json3CdnUrl BootcssCdn where
  urlJson3Js (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "json3/3.3.2/json3.min.js"
       else bootcssCdnUrl "json3/3.3.2/json3.js"


instance NodeUuidCdnUrl BootcssCdn where
  urlNodeUuidJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "node-uuid/1.4.8/uuid.min.js"
       else bootcssCdnUrl "node-uuid/1.4.8/uuid.js"


instance WeuiCdnUrl BootcssCdn where
  urlWeuiCss (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "weui/1.1.2/style/weui.min.css"
       else bootcssCdnUrl "weui/1.1.2/style/weui.css"


instance JqueryFancyTreeCdnUrl BootcssCdn where
  urlJqueryFancyTreeJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "jquery.fancytree/2.28.1/jquery.fancytree-all.min.js"
       else bootcssCdnUrl "jquery.fancytree/2.28.1/jquery.fancytree-all.js"

  urlJqueryFancyTreeSkinWin8Css (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "jquery.fancytree/2.28.1/skin-win8/ui.fancytree.min.css"
       else bootcssCdnUrl "jquery.fancytree/2.28.1/skin-win8/ui.fancytree.css"


instance JqueryLoadingOverlayCdnUrl BootcssCdn where
  urlJqueryLoadingOverlayJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "jquery-loading-overlay/2.1.3/loadingoverlay.min.js"
       else bootcssCdnUrl "jquery-loading-overlay/2.1.3/loadingoverlay.js"

instance TooltipsterCdnUrl BootcssCdn where
  urlTooltipsterJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "tooltipster/3.3.0/js/jquery.tooltipster.min.js"
       else bootcssCdnUrl "tooltipster/3.3.0/js/jquery.tooltipster.js"

  urlTooltipsterCss (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "tooltipster/3.3.0/css/tooltipster.min.css"
       else bootcssCdnUrl "tooltipster/3.3.0/css/tooltipster.css"

instance DropzoneCdnUrl BootcssCdn where
  urlDropzoneJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "dropzone/5.5.1/min/dropzone.min.js"
       else bootcssCdnUrl "dropzone/5.5.1/dropzone.js"

  urlDropzoneCss (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "dropzone/5.5.1/min/dropzone.min.css"
       else bootcssCdnUrl "dropzone/5.5.1/dropzone.css"

instance TimeagoCdnUrl BootcssCdn where
  urlTimeAgoJs (BootcssCdn min_ver) =
    if min_ver
       then bootcssCdnUrl "timeago.js/3.0.2/timeago.min.js"
       else bootcssCdnUrl "timeago.js/3.0.2/timeago.js"

  urlTimeAgoLocalesJs (BootcssCdn min_ver) =
    -- only min version available
    if min_ver
       then bootcssCdnUrl "timeago.js/3.0.2/timeago.locales.min.js"
       else bootcssCdnUrl "timeago.js/3.0.2/timeago.locales.min.js"

