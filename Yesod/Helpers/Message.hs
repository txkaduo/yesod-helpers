{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Helpers.Message where

import ClassyPrelude
import Yesod
import qualified Data.Text as T

import           Yesod.Form.I18n.Czech
import           Yesod.Form.I18n.Dutch
import           Yesod.Form.I18n.English
import           Yesod.Form.I18n.French
import           Yesod.Form.I18n.German
import           Yesod.Form.I18n.Japanese
import           Yesod.Form.I18n.Norwegian
import           Yesod.Form.I18n.Portuguese
import           Yesod.Form.I18n.Russian
import           Yesod.Form.I18n.Swedish

data YHCommon = YHCommon

mkMessage "YHCommon" "messages" "en"

deriving instance Show YHCommonMessage
deriving instance Eq YHCommonMessage

formMessageToYH :: FormMessage -> YHCommonMessage
formMessageToYH (MsgInvalidInteger x)    = MsgFormMsgInvalidInteger x
formMessageToYH (MsgInvalidNumber x)     = MsgFormMsgInvalidNumber x
formMessageToYH (MsgInvalidEntry x)      = MsgFormMsgInvalidEntry x
formMessageToYH (MsgInvalidUrl x)        = MsgFormMsgInvalidUrl x
formMessageToYH (MsgInvalidEmail x)      = MsgFormMsgInvalidEmail x
formMessageToYH MsgInvalidTimeFormat     = MsgFormMsgInvalidTimeFormat
formMessageToYH (MsgInvalidHour x)       = MsgFormMsgInvalidHour x
formMessageToYH (MsgInvalidMinute x)     = MsgFormMsgInvalidMinute x
formMessageToYH (MsgInvalidSecond x)     = MsgFormMsgInvalidSecond x
formMessageToYH MsgInvalidDay            = MsgFormMsgInvalidDay
formMessageToYH MsgCsrfWarning           = MsgFormMsgCsrfWarning
formMessageToYH MsgValueRequired         = MsgFormMsgValueRequired
formMessageToYH (MsgInputNotFound x)     = MsgFormMsgInputNotFound x
formMessageToYH MsgSelectNone            = MsgFormMsgSelectNone
formMessageToYH (MsgInvalidBool x)       = MsgFormMsgInvalidBool x
formMessageToYH MsgBoolYes               = MsgFormMsgBoolYes
formMessageToYH MsgBoolNo                = MsgFormMsgBoolNo
formMessageToYH MsgDelete                = MsgFormMsgDelete


renderFormMessageByYHCommonMessage :: [Lang] -> FormMessage -> Text
renderFormMessageByYHCommonMessage [] fm = defaultFormMessage fm
renderFormMessageByYHCommonMessage all_langs@(lang:langs) fm =
  case main_locale of
    "zh" -> renderMessage YHCommon all_langs (formMessageToYH fm)
    "en" -> englishFormMessage fm
    "ja" -> japaneseFormMessage fm
    "fr" -> frenchFormMessage fm
    "de" -> germanFormMessage fm
    "cs" -> czechFormMessage fm
    "nl" -> dutchFormMessage fm
    "nb" -> norwegianBokmålFormMessage fm
    "no" -> norwegianBokmålFormMessage fm
    "nn" -> norwegianBokmålFormMessage fm
    "pt" -> portugueseFormMessage fm
    "ru" -> russianFormMessage fm
    "sv" -> swedishFormMessage fm
    _    -> renderFormMessageByYHCommonMessage langs fm

  where main_locale = fst $ T.breakOn "-" lang


weekDayMessage :: Int -> Maybe YHCommonMessage
weekDayMessage 1 = Just MsgMonday
weekDayMessage 2 = Just MsgTuesday
weekDayMessage 3 = Just MsgWednesday
weekDayMessage 4 = Just MsgThursday
weekDayMessage 5 = Just MsgFriday
weekDayMessage 6 = Just MsgSaturday
weekDayMessage 7 = Just MsgSunday
weekDayMessage _ = Nothing

