module Yesod.Helpers.Yaml (
    module Yesod.Helpers.Yaml
    , DefaultEnv(..)
    ) where

import Prelude
import Data.Yaml
import Yesod
import Data.Text                            (Text)
import Data.Aeson                           (withObject)
import qualified Data.Text                  as T
import Data.Maybe                           (isJust)
import Data.List                            (find)
import Yesod.Default.Config                 (DefaultEnv(..))

import Yesod.Helpers.Aeson


-- | check whether a string is in the list identified by a key string
-- some special strings:
-- __any__ will match any string
--
-- example yaml like this
-- Develpment:
--   dangerous-op: foo bar
--   simple-op:
--     - foo
--     - bar
--   safe-op: __any__
checkInListYaml ::
    (Show config, MonadIO m, MonadLogger m) =>
    FilePath
    -> config       -- ^ config section: Develpment/Production/...
    -> Text         -- ^ the key
    -> Text         -- ^ the name looking for
    -> m Bool
checkInListYaml fp c key name = do
    checkInListYaml' fp c key (== name)


checkInListYaml' ::
    (Show config, MonadIO m, MonadLogger m) =>
    FilePath
    -> config       -- ^ config section: Develpment/Production/...
    -> Text         -- ^ the key
    -> (Text -> Bool)   -- ^ the name looking for
    -> m Bool
checkInListYaml' fp c key chk_name = do
    result <- liftIO $ decodeFileEither fp
    case result of
        Left ex -> do
                    $(logError) $ T.pack $
                        "failed to parse YAML file " ++ show fp ++ ": " ++ show ex
                    return False
        Right v -> do
                    case parseEither look_for_names v of
                        Left err -> do
                                $(logError) $ T.pack $
                                    "YAML " ++ show fp
                                            ++ " contains invalid content: "
                                            ++ show err
                                return False
                        Right names -> return $ isJust $ find match names
    where
        look_for_names = withObject "section-mapping" $ \obj -> do
            obj .:? (T.pack $ show c)
                >>= maybe (return [])
                    (\section -> do
                        section .:? key >>=
                                    maybe (return []) (parseWordList' "words")
                    )

        match x = if x == "__any__"
                    then True
                    else chk_name x
