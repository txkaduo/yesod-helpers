module Yesod.Helpers.ParamNames where

import ClassyPrelude


loginParamNames :: [Text]
loginParamNames =   [ savedReqStateParamName
                    , returnUrlParamName
                    , returnUrl2ParamName
                    , loginMsgParamName
                    ]

loginMsgParamName :: Text
loginMsgParamName = "login_msg"

returnUrlParamName :: Text
returnUrlParamName = "return_url"

-- | 这是打算用于登录取消或失败时返回的地址，但这里的代码暂时没有直接用到
-- 定义这个变量名是方便多项目统一
returnUrl2ParamName :: Text
returnUrl2ParamName = "return_url2"


-- | use this name to pass get parameter, value of which is save state key
savedReqStateParamName :: IsString a => a
savedReqStateParamName = fromString "_SRS"


