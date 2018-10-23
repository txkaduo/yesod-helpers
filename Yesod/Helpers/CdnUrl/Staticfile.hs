{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl.Staticfile
  ( StaticFileCdn(..) )
  where

import Yesod.Helpers.CdnUrl.Class

data StaticFileCdn = StaticFileCdn Bool

staticfileCdnUrl = (<>) "https://cdn.staticfile.org/"

instance JqueryCdnUrl StaticFileCdn where
    urlJqueryJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "jquery/1.12.4/jquery.min.js"
            else staticfileCdnUrl "jquery/1.12.4/jquery.js"

    urlJqueryUiJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "jqueryui/1.12.1/jquery-ui.min.js"
            else staticfileCdnUrl "jqueryui/1.12.1/jquery-ui.js"

    urlJqueryUiCssText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "jqueryui/1.12.1/jquery-ui.min.css"
            else staticfileCdnUrl "jqueryui/1.12.1/jquery-ui.css"


instance JqueryFormCdnUrl StaticFileCdn where
    urlJqueryFormJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "jquery.form/4.2.2/jquery.form.min.js"
            else staticfileCdnUrl "jquery.form/4.2.2/jquery.form.js"


instance JqueryQrcodeCdnUrl StaticFileCdn where
  urlJqueryQrcodeJs (StaticFileCdn _min_ver) =
    staticfileCdnUrl "jquery.qrcode/1.0/jquery.qrcode.min.js"


instance BootstrapCdnUrl StaticFileCdn where
    urlBootstrapCssText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "twitter-bootstrap/3.3.7/css/bootstrap.min.css"
            else staticfileCdnUrl "twitter-bootstrap/3.3.7/css/bootstrap.css"

    urlBootstrapJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "twitter-bootstrap/3.3.7/js/bootstrap.min.js"
            else staticfileCdnUrl "twitter-bootstrap/3.3.7/js/bootstrap.js"


instance FontAwesomeCdnUrl StaticFileCdn where
    urlFontAwesomeCssText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "font-awesome/4.7.0/css/font-awesome.min.css"
            else staticfileCdnUrl "font-awesome/4.7.0/css/font-awesome.css"

    urlFontAwesomeWebFontText _ =
        staticfileCdnUrl "font-awesome/4.7.0/fonts/fontawesome-webfont.svg"


instance HandlebarsCdnUrl StaticFileCdn where
    urlHandlebarsJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "handlebars.js/4.0.12/handlebars.min.js"
            else staticfileCdnUrl "handlebars.js/4.0.12/handlebars.js"

    urlHandlebarsRuntimeJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "handlebars.js/4.0.12/handlebars.runtime.min.js"
            else staticfileCdnUrl "handlebars.js/4.0.12/handlebars.runtime.js"


instance ZeptoCdnUrl StaticFileCdn where
    urlZeptoJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "zepto/1.1.7/zepto.min.js"
            else staticfileCdnUrl "zepto/1.1.7/zepto.js"

instance OnePageScrollCssCdnUrl StaticFileCdn where
    urlOnePageScrollCssText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "onepage-scroll/1.3.1/onepage-scroll.min.css"
            else staticfileCdnUrl "onepage-scroll/1.3.1/onepage-scroll.css"

instance OnePageScrollJsJqueryCdnUrl StaticFileCdn where
    urlOnePageScrollJsJqueryText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "onepage-scroll/1.3.1/jquery.onepage-scroll.min.js"
            else staticfileCdnUrl "onepage-scroll/1.3.1/jquery.onepage-scroll.js"

instance JsCookieCdnUrl StaticFileCdn where
    urlJsCookieJsText (StaticFileCdn min_ver) =
        if min_ver
            then staticfileCdnUrl "js-cookie/latest/js.cookie.min.js"
            else staticfileCdnUrl "js-cookie/latest/js.cookie.js"

instance TypeaheadCdnUrl StaticFileCdn where
  urlTypeaheadBundleJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "typeahead.js/0.11.1/typeahead.bundle.min.js"
       else staticfileCdnUrl "typeahead.js/0.11.1/typeahead.bundle.js"


instance VideoJsCdnUrl StaticFileCdn where
  urlVideoJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "video.js/6.12.1/video.min.js"
       else staticfileCdnUrl "video.js/6.12.1/video.js"

  urlVideoJsCss (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "video.js/6.12.1/video-js.min.css"
       else staticfileCdnUrl "video.js/6.12.1/video-js.css"


instance VideoJsOverlayCdnUrl StaticFileCdn where
  urlVideoJsOverlay (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "videojs-overlay/1.1.4/videojs-overlay.min.js"
       else staticfileCdnUrl "videojs-overlay/1.1.4/videojs-overlay.js"

  urlVideoJsOverlayCss (StaticFileCdn _min_ver) =
    staticfileCdnUrl "videojs-overlay/1.1.4/videojs-overlay.css"


instance ClipboardJsCdnUrl StaticFileCdn where
  urlClipboardJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "clipboard.js/1.7.1/clipboard.min.js"
       else staticfileCdnUrl "clipboard.js/1.7.1/clipboard.js"


instance FileSaverCdnUrl StaticFileCdn where
  urlFileSaverJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "FileSaver.js/1.3.8/FileSaver.min.js"
       else staticfileCdnUrl "FileSaver.js/1.3.8/FileSaver.js"


instance JsXlsxCdnUrl StaticFileCdn where
  urlJsXlsxCore (StaticFileCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then staticfileCdnUrl "xlsx/0.14.0/xlsx.core.min.js"
       else staticfileCdnUrl "xlsx/0.14.0/xlsx.core.min.js"

  urlJsXlsxFull (StaticFileCdn min_ver) =
    -- only 'min' version available
    if min_ver
       then staticfileCdnUrl "xlsx/0.14.0/xlsx.full.min.js"
       else staticfileCdnUrl "xlsx/0.14.0/xlsx.full.min.js"


instance TableExportCdlUrl StaticFileCdn where
  urlTableExportJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "TableExport/5.0.2/js/tableexport.min.js"
       else staticfileCdnUrl "TableExport/5.0.2/js/tableexport.js"

  urlTableExportCss (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "TableExport/5.0.2/css/tableexport.min.css"
       else staticfileCdnUrl "TableExport/5.0.2/css/tableexport.css"


instance MomentCdnUrl StaticFileCdn where
  urlMoment (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "moment.js/2.22.2/moment.min.js"
       else staticfileCdnUrl "moment.js/2.22.2/moment.js"

  urlMomentWithAllLocale (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "moment.js/2.22.2/moment-with-locales.min.js"
       else staticfileCdnUrl "moment.js/2.22.2/moment-with-locales.js"


instance SummerNoteCdnUrl StaticFileCdn where
  urlSummerNoteJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "summernote/0.8.10/summernote.min.js"
       else staticfileCdnUrl "summernote/0.8.10/summernote.js"

  urlSummerNoteJsZhCn (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "summernote/0.8.10/lang/summernote-zh-CN.min.js"
       else staticfileCdnUrl "summernote/0.8.10/lang/summernote-zh-CN.js"

  urlSummerNoteCss (StaticFileCdn _min_ver) =
    staticfileCdnUrl "summernote/0.8.10/summernote.css"


instance PluploadCdnUrl StaticFileCdn where
  urlPluploadFullJs (StaticFileCdn _min_ver) =
    staticfileCdnUrl "plupload/2.3.6/plupload.full.min.js"

  urlPluploadJsZhCn (StaticFileCdn _min_ver) =
    staticfileCdnUrl "plupload/2.3.6/i18n/zh_CN.js"

  urlPluploadUiJqueryJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.min.js"
       else staticfileCdnUrl "plupload/2.3.6/jquery.ui.plupload/jquery.ui.plupload.js"

  urlPluploadUiJqueryCss (StaticFileCdn _min_ver) =
    staticfileCdnUrl "plupload/2.3.6/jquery.plupload.queue/css/jquery.plupload.queue.css"


instance MoxieCdnUrl StaticFileCdn where
  urlMoxieSwf (StaticFileCdn _) =
    staticfileCdnUrl "plupload/2.3.6/Moxie.swf"


instance Json3CdnUrl StaticFileCdn where
  urlJson3Js (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "json3/3.3.2/json3.min.js"
       else staticfileCdnUrl "json3/3.3.2/json3.js"


instance NodeUuidCdnUrl StaticFileCdn where
  urlNodeUuidJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "node-uuid/1.4.8/uuid.min.js"
       else staticfileCdnUrl "node-uuid/1.4.8/uuid.js"


instance WeuiCdnUrl StaticFileCdn where
  urlWeuiCss (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "weui/1.1.3/style/weui.min.css"
       else staticfileCdnUrl "weui/1.1.3/style/weui.css"


instance JqueryFancyTreeCdnUrl StaticFileCdn where
  urlJqueryFancyTreeJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "jquery.fancytree/2.30.0/jquery.fancytree-all.min.js"
       else staticfileCdnUrl "jquery.fancytree/2.30.0/jquery.fancytree-all.js"

  urlJqueryFancyTreeSkinWin8Css (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "jquery.fancytree/2.30.0/skin-win8/ui.fancytree.min.css"
       else staticfileCdnUrl "jquery.fancytree/2.30.0/skin-win8/ui.fancytree.css"


instance JqueryLoadingOverlayCdnUrl StaticFileCdn where
  urlJqueryLoadingOverlayJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "jquery-loading-overlay/2.1.6/loadingoverlay.min.js"
       else staticfileCdnUrl "jquery-loading-overlay/2.1.6/loadingoverlay.js"


instance TooltipsterCdnUrl StaticFileCdn where
  urlTooltipsterJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "tooltipster/3.3.0/js/jquery.tooltipster.min.js"
       else staticfileCdnUrl "tooltipster/3.3.0/js/jquery.tooltipster.js"

  urlTooltipsterCss (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "tooltipster/3.3.0/css/tooltipster.min.css"
       else staticfileCdnUrl "tooltipster/3.3.0/css/tooltipster.css"

instance DropzoneCdnUrl StaticFileCdn where
  urlDropzoneJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "dropzone/5.5.1/min/dropzone.min.js"
       else staticfileCdnUrl "dropzone/5.5.1/dropzone.js"

  urlDropzoneCss (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "dropzone/5.5.1/min/dropzone.min.css"
       else staticfileCdnUrl "dropzone/5.5.1/dropzone.css"

instance TimeagoCdnUrl StaticFileCdn where
  urlTimeAgoJs (StaticFileCdn min_ver) =
    if min_ver
       then staticfileCdnUrl "timeago.js/3.0.2/timeago.min.js"
       else staticfileCdnUrl "timeago.js/3.0.2/timeago.js"

  urlTimeAgoLocalesJs (StaticFileCdn min_ver) =
    -- only min version available
    if min_ver
       then staticfileCdnUrl "timeago.js/3.0.2/timeago.locales.min.js"
       else staticfileCdnUrl "timeago.js/3.0.2/timeago.locales.min.js"

