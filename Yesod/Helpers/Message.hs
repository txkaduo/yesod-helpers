{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Yesod.Helpers.Message where

import Prelude
import Data.Text                            (Text)

import Text.Shakespeare.I18N                (mkMessage, renderMessage)


asText :: Text -> Text
asText = id

data YHCommon = YHCommon

mkMessage "YHCommon" "messages" "en"

deriving instance Show YHCommonMessage
deriving instance Eq YHCommonMessage
