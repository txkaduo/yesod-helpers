{-# LANGUAGE OverloadedStrings #-}
module Yesod.Helpers.CdnUrl.Class
  ( module Yesod.Helpers.CdnUrl.Class
  , Bool, (<>)
  )
  where

import Prelude
import Data.Semigroup                       ((<>))
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


class ReconnectingWebSocketCdnUrl a where
  urlReconnectingWebSocketJs :: a -> Text


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
                          , ReconnectingWebSocketCdnUrl a
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

instance ReconnectingWebSocketCdnUrl SomeFullStaticCdnUrl where
  urlReconnectingWebSocketJs (SomeFullStaticCdnUrl x) = urlReconnectingWebSocketJs x
