{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl.JsDelivr
  ( JsDelivrCdn(..) )
  where

import Yesod.Helpers.CdnUrl.Class
import Data.Text (Text)

data JsDelivrCdn = JsDelivrCdn Bool

jsDelivrCdnUrl :: Text -> Text
jsDelivrCdnUrl = (<>) "https://cdn.jsdelivr.net/npm/"

instance JqueryCdnUrl JsDelivrCdn where
    urlJqueryJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "jquery@1.12.4/dist/jquery.min.js"
            else jsDelivrCdnUrl "jquery@1.12.4/dist/jquery.js"

    urlJqueryUiJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "jquery-ui-dist@1.12.1/jquery-ui.min.js"
            else jsDelivrCdnUrl "jquery-ui-dist@1.12.1/jquery-ui.js"

    urlJqueryUiCssText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "jquery-ui-dist@1.12.1/jquery-ui.min.css"
            else jsDelivrCdnUrl "jquery-ui-dist@1.12.1/jquery-ui.css"


instance JqueryFormCdnUrl JsDelivrCdn where
    urlJqueryFormJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "jquery-form@4.2.2/dist/jquery.form.min.js"
            else jsDelivrCdnUrl "jquery-form@4.2.2/dist/jquery.form.js"


instance JqueryQrcodeCdnUrl JsDelivrCdn where
  urlJqueryQrcodeJs (JsDelivrCdn _min_ver) =
    jsDelivrCdnUrl "jquery.qrcode@1.0.3/jquery.qrcode.min.js"


instance BootstrapCdnUrl JsDelivrCdn where
    urlBootstrapCssText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "bootstrap@3.3.7/dist/css/bootstrap.min.css"
            else jsDelivrCdnUrl "bootstrap@3.3.7/dist/css/bootstrap.css"

    urlBootstrapJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "bootstrap@3.3.7/dist/js/bootstrap.min.js"
            else jsDelivrCdnUrl "bootstrap@3.3.7/dist/js/bootstrap.js"


instance FontAwesomeCdnUrl JsDelivrCdn where
    urlFontAwesomeCssText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "font-awesome@4.7.0/css/font-awesome.min.css"
            else jsDelivrCdnUrl "font-awesome@4.7.0/css/font-awesome.css"

    urlFontAwesomeWebFontText _ =
        jsDelivrCdnUrl "font-awesome@4.7.0/fonts/fontawesome-webfont.svg"


instance HandlebarsCdnUrl JsDelivrCdn where
    urlHandlebarsJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "handlebars@4.0.12/dist/handlebars.min.js"
            else jsDelivrCdnUrl "handlebars@4.0.12/dist/handlebars.js"

    urlHandlebarsRuntimeJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "handlebars@4.0.12/dist/handlebars.runtime.min.js"
            else jsDelivrCdnUrl "handlebars@4.0.12/dist/handlebars.runtime.js"


instance ZeptoCdnUrl JsDelivrCdn where
    urlZeptoJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "zepto@1.1.7/dist/zepto.min.js"
            else jsDelivrCdnUrl "zepto@1.1.7/dist/zepto.js"

instance OnePageScrollCssCdnUrl JsDelivrCdn where
    urlOnePageScrollCssText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "onepage-scroll@1.3.1/onepage-scroll.min.css"
            else jsDelivrCdnUrl "onepage-scroll@1.3.1/onepage-scroll.css"

instance OnePageScrollJsJqueryCdnUrl JsDelivrCdn where
    urlOnePageScrollJsJqueryText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "onepage-scroll@1.3.1/jquery.onepage-scroll.min.js"
            else jsDelivrCdnUrl "onepage-scroll@1.3.1/jquery.onepage-scroll.js"

instance JsCookieCdnUrl JsDelivrCdn where
    urlJsCookieJsText (JsDelivrCdn min_ver) =
        if min_ver
            then jsDelivrCdnUrl "js-cookie@2.2.0/src/js.cookie.min.js"
            else jsDelivrCdnUrl "js-cookie@2.2.0/src/js.cookie.js"

instance TypeaheadCdnUrl JsDelivrCdn where
  urlTypeaheadBundleJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "typeahead.js@0.11.1/dist/typeahead.bundle.min.js"
       else jsDelivrCdnUrl "typeahead.js@0.11.1/dist/typeahead.bundle.js"


instance VideoJsCdnUrl JsDelivrCdn where
  urlVideoJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "video.js@6.12.1/dist/video.min.js"
       else jsDelivrCdnUrl "video.js@6.12.1/dist/video.js"

  urlVideoJsCss (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "video.js@6.12.1/dist/video-js.min.css"
       else jsDelivrCdnUrl "video.js@6.12.1/dist/video-js.css"


instance VideoJsOverlayCdnUrl JsDelivrCdn where
  urlVideoJsOverlay (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "videojs-overlay@1.1.4/dist/videojs-overlay.min.js"
       else jsDelivrCdnUrl "videojs-overlay@1.1.4/dist/videojs-overlay.js"

  urlVideoJsOverlayCss (JsDelivrCdn _min_ver) =
    jsDelivrCdnUrl "videojs-overlay@1.1.4/dist/videojs-overlay.css"


instance ClipboardJsCdnUrl JsDelivrCdn where
  urlClipboardJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "clipboard@1.7.1/dist/clipboard.min.js"
       else jsDelivrCdnUrl "clipboard@1.7.1/dist/clipboard.js"


instance FileSaverCdnUrl JsDelivrCdn where
  urlFileSaverJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "file-saver@1.3.8/FileSaver.min.js"
       else jsDelivrCdnUrl "file-saver@1.3.8/FileSaver.js"


instance JsXlsxCdnUrl JsDelivrCdn where
  urlJsXlsxCore (JsDelivrCdn _min_ver) =
    -- only 'min' version available
    jsDelivrCdnUrl "xlsx@0.14.0/dist/xlsx.core.min.js"

  urlJsXlsxFull (JsDelivrCdn _min_ver) =
    -- only 'min' version available
    jsDelivrCdnUrl "xlsx@0.14.0/dist/xlsx.full.min.js"


instance TableExportCdlUrl JsDelivrCdn where
  urlTableExportJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "tableexport@5.0.2/dist/js/tableexport.min.js"
       else jsDelivrCdnUrl "tableexport@5.0.2/dist/js/tableexport.js"

  urlTableExportCss (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "tableexport@5.0.2/dist/css/tableexport.min.css"
       else jsDelivrCdnUrl "tableexport@5.0.2/dist/css/tableexport.css"


instance MomentCdnUrl JsDelivrCdn where
  urlMoment (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "moment@2.22.2/moment.min.js"
       else jsDelivrCdnUrl "moment@2.22.2/moment.js"

  urlMomentWithAllLocale (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "moment@2.22.2/min/moment-with-locales.min.js"
       else jsDelivrCdnUrl "moment@2.22.2/min/moment-with-locales.js"


instance SummerNoteCdnUrl JsDelivrCdn where
  urlSummerNoteJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "summernote@0.8.10/dist/summernote.min.js"
       else jsDelivrCdnUrl "summernote@0.8.10/dist/summernote.js"

  urlSummerNoteJsZhCn (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "summernote@0.8.10/dist/lang/summernote-zh-CN.min.js"
       else jsDelivrCdnUrl "summernote@0.8.10/dist/lang/summernote-zh-CN.js"

  urlSummerNoteCss (JsDelivrCdn _min_ver) =
    jsDelivrCdnUrl "summernote@0.8.10/dist/summernote.css"


instance PluploadCdnUrl JsDelivrCdn where
  urlPluploadFullJs (JsDelivrCdn _min_ver) =
    jsDelivrCdnUrl "plupload@2.3.6/js/plupload.full.min.js"

  urlPluploadJsZhCn (JsDelivrCdn _min_ver) =
    jsDelivrCdnUrl "plupload@2.3.6/i18n/zh_CN.js"

  urlPluploadUiJqueryJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "plupload@2.3.6/js/jquery.ui.plupload/jquery.ui.plupload.min.js"
       else jsDelivrCdnUrl "plupload@2.3.6/js/jquery.ui.plupload/jquery.ui.plupload.js"

  urlPluploadUiJqueryCss (JsDelivrCdn _min_ver) =
    jsDelivrCdnUrl "plupload@2.3.6/js/jquery.ui.plupload/css/jquery.ui.plupload.css"


instance MoxieCdnUrl JsDelivrCdn where
  urlMoxieSwf (JsDelivrCdn _) =
    jsDelivrCdnUrl "plupload@2.3.6/js/Moxie.swf"


instance Json3CdnUrl JsDelivrCdn where
  urlJson3Js (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "json3@3.3.2/lib/json3.min.js"
       else jsDelivrCdnUrl "json3@3.3.2/lib/json3.js"


instance NodeUuidCdnUrl JsDelivrCdn where
  urlNodeUuidJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "node-uuid@1.4.8/uuid.min.js"
       else jsDelivrCdnUrl "node-uuid@1.4.8/uuid.js"


instance WeuiCdnUrl JsDelivrCdn where
  urlWeuiCss (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "weui@1.1.3/dist/style/weui.min.css"
       else jsDelivrCdnUrl "weui@1.1.3/dist/style/weui.css"


instance JqueryFancyTreeCdnUrl JsDelivrCdn where
  urlJqueryFancyTreeJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "jquery.fancytree@2.30.0/dist/jquery.fancytree-all.min.js"
       else jsDelivrCdnUrl "jquery.fancytree@2.30.0/dist/jquery.fancytree-all.js"

  urlJqueryFancyTreeSkinWin8Css (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "jquery.fancytree@2.30.0/dist/skin-win8/ui.fancytree.min.css"
       else jsDelivrCdnUrl "jquery.fancytree@2.30.0/dist/skin-win8/ui.fancytree.css"


instance JqueryLoadingOverlayCdnUrl JsDelivrCdn where
  urlJqueryLoadingOverlayJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "gasparesganga-jquery-loading-overlay@2.1.6/dist/loadingoverlay.min.js"
       else jsDelivrCdnUrl "gasparesganga-jquery-loading-overlay@2.1.6/dist/loadingoverlay.js"


instance TooltipsterCdnUrl JsDelivrCdn where
  urlTooltipsterJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "jquery-tooltipster@3.3.0/js/jquery.tooltipster.min.js"
       else jsDelivrCdnUrl "jquery-tooltipster@3.3.0/js/jquery.tooltipster.js"

  urlTooltipsterCss (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "jquery-tooltipster@3.3.0/css/tooltipster.min.css"
       else jsDelivrCdnUrl "jquery-tooltipster@3.3.0/css/tooltipster.css"

instance DropzoneCdnUrl JsDelivrCdn where
  urlDropzoneJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "dropzone@5.5.1/dist/min/dropzone.min.js"
       else jsDelivrCdnUrl "dropzone@5.5.1/dist/dropzone.js"

  urlDropzoneCss (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "dropzone@5.5.1/dist/min/dropzone.min.css"
       else jsDelivrCdnUrl "dropzone@5.5.1/dist/dropzone.css"

instance TimeagoCdnUrl JsDelivrCdn where
  urlTimeAgoJs (JsDelivrCdn min_ver) =
    if min_ver
       then jsDelivrCdnUrl "timeago.js@3.0.2/dist/timeago.min.js"
       else jsDelivrCdnUrl "timeago.js@3.0.2/dist/timeago.js"

  urlTimeAgoLocalesJs (JsDelivrCdn min_ver) =
    -- only min version available
    if min_ver
       then jsDelivrCdnUrl "timeago.js@3.0.2/dist/timeago.locales.min.js"
       else jsDelivrCdnUrl "timeago.js@3.0.2/dist/timeago.locales.min.js"

