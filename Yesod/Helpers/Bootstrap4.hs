{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeFamilies      #-}
-- | this program based on Yesod.Form.Bootstrap3 of yesod-form
-- yesod-form under MIT license, author is Michael Snoyman <michael@snoyman.com>
--
-- base on yesod-form-bootstrap4-3.0.0
-- update according to latest bootstrap4
module Yesod.Helpers.Bootstrap4
  ( renderBootstrap4
  , BootstrapFormLayout(..)
  , BootstrapGridOptions(..)
  , bfs
  , bfsFile
  , withPlaceholder
  , withAutofocus
  , withLargeInput
  , withSmallInput
  , bootstrapSubmit
  , mbootstrapSubmit
  , BootstrapSubmit(..)
  , radioFieldBs4, radioFieldListBs4
  , boolFieldBs4, checkBoxFieldBs4
  , checkboxesFieldBs4, checkboxesFieldListBs4
  ) where

import           ClassyPrelude
import qualified Data.Text.Lazy                as TL
import           Text.Blaze.Html.Renderer.Text
import           Yesod.Core
import           Yesod.Form

import           Yesod.Compat

bfs :: RenderMessage site msg => msg -> FieldSettings site
bfs msg
  = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "form-control")]

bfsFile :: RenderMessage site msg => msg -> FieldSettings site
bfsFile msg
  = FieldSettings (SomeMessage msg) Nothing Nothing Nothing [("class", "form-control-file")]

withPlaceholder :: Text -> FieldSettings site -> FieldSettings site
withPlaceholder placeholder fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("placeholder", placeholder) : fsAttrs fs

-- | Add an autofocus attribute to a field.
withAutofocus :: FieldSettings site -> FieldSettings site
withAutofocus fs = fs { fsAttrs = newAttrs }
  where newAttrs = ("autofocus", "autofocus") : fsAttrs fs

-- | Add the @input-lg@ CSS class to a field.
withLargeInput :: FieldSettings site -> FieldSettings site
withLargeInput fs = fs { fsAttrs = newAttrs }
  where newAttrs = addClass "form-control-lg" (fsAttrs fs)

-- | Add the @input-sm@ CSS class to a field.
withSmallInput :: FieldSettings site -> FieldSettings site
withSmallInput fs = fs { fsAttrs = newAttrs }
  where newAttrs = addClass "form-control-sm" (fsAttrs fs)

data BootstrapGridOptions = ColXs !Int | ColSm !Int | ColMd !Int | ColLg !Int | ColXl !Int
  deriving (Eq, Ord, Show, Read)

toColumn :: BootstrapGridOptions -> String
toColumn (ColXs columns) = "col-xs-" ++ show columns
toColumn (ColSm columns) = "col-sm-" ++ show columns
toColumn (ColMd columns) = "col-md-" ++ show columns
toColumn (ColLg columns) = "col-lg-" ++ show columns
toColumn (ColXl columns) = "col-xl-" ++ show columns

toOffset :: BootstrapGridOptions -> String
toOffset (ColXs columns) = "offset-xs-" ++ show columns
toOffset (ColSm columns) = "offset-sm-" ++ show columns
toOffset (ColMd columns) = "offset-md-" ++ show columns
toOffset (ColLg columns) = "offset-lg-" ++ show columns
toOffset (ColXl columns) = "offset-xl-" ++ show columns

addGO :: BootstrapGridOptions -> BootstrapGridOptions -> BootstrapGridOptions
addGO (ColXs a) (ColXs b) = ColXs (a+b)
addGO (ColSm a) (ColSm b) = ColSm (a+b)
addGO (ColMd a) (ColMd b) = ColMd (a+b)
addGO (ColLg a) (ColLg b) = ColLg (a+b)
addGO a b                 | a > b = addGO b a
addGO (ColXs a) other     = addGO (ColSm a) other
addGO (ColSm a) other     = addGO (ColMd a) other
addGO (ColMd a) other     = addGO (ColLg a) other
addGO _     _             = error "Yesod.Form.Bootstrap.addGO: never here"

-- | The layout used for the bootstrap form.
data BootstrapFormLayout = BootstrapBasicForm | BootstrapInlineForm |
  BootstrapHorizontalForm
  { bflLabelOffset :: !BootstrapGridOptions
  , bflLabelSize   :: !BootstrapGridOptions
  , bflInputOffset :: !BootstrapGridOptions
  , bflInputSize   :: !BootstrapGridOptions
  }
  deriving (Eq, Ord, Show, Read)


data FormInputType = FormInputRadio
                   | FormInputCheckbox
                   | FormInputOther
                   deriving (Eq, Ord, Enum, Bounded, Show)

inputType :: Yesod site => FieldView site -> HandlerOf site FormInputType
inputType view = do
  textCode <- fiedViewHtmlCode view
  pure $
    case () of
      () | "\"radio\"" `TL.isInfixOf` textCode -> FormInputRadio
         | "\"checkbox\"" `TL.isInfixOf` textCode -> FormInputCheckbox
         | otherwise -> FormInputOther


fiedViewHtmlCode :: Yesod site => FieldView site -> HandlerOf site TL.Text
fiedViewHtmlCode view = fmap renderHtml $ to_body_html (fvInput view)
  where to_body_html widget = widgetToPageContent widget >>= withUrlRenderer . pageBody


-- | Render the given form using Bootstrap v4 conventions.
renderBootstrap4 :: (MonadHandler m, Yesod (HandlerSite m)) => BootstrapFormLayout -> FormRender m a
renderBootstrap4 formLayout aform fragment = do
  (res, views') <- aFormToForm aform
  let views = views' []

  views_and_type <-
    liftMonadHandler $ do
      forM views $ \ view -> do
        it <- inputType view
        pure (view, it)

  let widget = [whamlet|
#{fragment}
$forall (view, it) <- views_and_type
  $case it
    $of FormInputRadio
      ^{renderInputRadio view formLayout}
    $of FormInputCheckbox
      ^{renderInputCheckbox view formLayout}
    $of FormInputOther
      ^{renderInputOther view formLayout}
|]
  return (res, widget)


renderInputRadio :: FieldView site -> BootstrapFormLayout -> WidgetFor site ()
renderInputRadio view formLayout = [whamlet|
$case formLayout
  $of BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    <div .form-group .row>
      <legend .col-form-label .#{toOffset labelOffset} .#{toColumn labelSize} :is_invalid:.is-invalid>
        #{fvLabel view}
      <fieldset .#{toOffset inputOffset} .#{toColumn inputSize}>
        ^{fvInput view}
        ^{helpWidget view}

  $of BootstrapInlineForm
    <legend .d-flex .w-auto>
      #{fvLabel view}
    <fieldset :is_invalid:.is-invalid>
      ^{fvInput view}
      ^{helpWidget view}
  $of _
    <fieldset :is_invalid:.is-invalid>
      ^{fvInput view}
      ^{helpWidget view}
|]
  where is_invalid = isJust $ fvErrors view


-- | Render checkBoxFieldBs4 and checkboxesFieldBs4 differently.
-- checkBoxFieldBs4: view 只有 <input>，我们在后面直接把label加上去
-- checkboxesFieldBs4: view 有很多个 <input><label>已包装在 <div.form-check> 里，这个field的label就要放在旁边
renderInputCheckbox :: Yesod site => FieldView site -> BootstrapFormLayout -> WidgetFor site ()
renderInputCheckbox view formLayout = do
  textCode <- liftMonadHandler $ fiedViewHtmlCode view
  [whamlet|
$case formLayout
  $of BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    <div .form-group .row>
      $if TL.isInfixOf "form-check-label" textCode
        <div
          .col-form-label
          .#{toOffset labelOffset}
          .#{toColumn labelSize}
          for=#{fvId view}
          >#{fvLabel view}
        <div .#{toOffset inputOffset} .#{toColumn inputSize} ##{fvId view}>
          ^{fvInput view}
          ^{helpWidget view}
      $else
        <div
          .col-form-label
          .#{toOffset labelOffset}
          .#{toColumn labelSize}
          >
        <div .#{toOffset inputOffset} .#{toColumn inputSize}>
          <div .form-check :is_invalid:.is-invalid>
            ^{fvInput view}
            <label for=#{fvId view} .form-check-label>#{fvLabel view}
            ^{helpWidget view}

  $of _
    $if TL.isInfixOf "form-check-label" textCode
      <label .sr-only for=#{fvId view}>#{fvLabel view}
      ^{fvInput view}
      ^{helpWidget view}
    $else
      <div .form-check :is_invalid:.is-invalid>
        ^{fvInput view}
        <label for=#{fvId view} .form-check-label>#{fvLabel view}
        ^{helpWidget view}
|]
  where is_invalid = isJust $ fvErrors view


renderInputOther :: FieldView site -> BootstrapFormLayout -> WidgetFor site ()
renderInputOther view formLayout = [whamlet|
$case formLayout
  $of BootstrapBasicForm
    $if fvId view /= bootstrapSubmitId
      <label for=#{fvId view}>#{fvLabel view}
    ^{fvInput view}
    ^{helpWidget view}
  $of BootstrapInlineForm
    $if fvId view /= bootstrapSubmitId
      <label .sr-only for=#{fvId view}>#{fvLabel view}
    ^{fvInput view}
    ^{helpWidget view}
  $of BootstrapHorizontalForm labelOffset labelSize inputOffset inputSize
    $if fvId view /= bootstrapSubmitId
      <div .row .form-group>
        <label
          .col-form-label
          .#{toOffset labelOffset}
          .#{toColumn labelSize}
          for=#{fvId view}>#{fvLabel view}
        <div .#{toOffset inputOffset} .#{toColumn inputSize}>
          ^{fvInput view}
          ^{helpWidget view}
    $else
      <div
        .#{toOffset (addGO inputOffset (addGO labelOffset labelSize))}
        .#{toColumn inputSize}>
        ^{fvInput view}
        ^{helpWidget view}
|]


-- | (Internal) Render a help widget for tooltips and errors.
-- .invalid-feedbackを必ず表示する
-- bootstrap 4.1の書式ではinputがerrorでなければエラーメッセージが出ませんが
-- yesod-formのAPIではfvInput自体を弄るのが困難ですし
-- yesod-formのAPI上fvErrorsが存在する時は常にエラーメッセージは表示させるべきなので汚いやり方ですが
-- styleを上書きして常に表示します
helpWidget :: FieldView site -> WidgetFor site ()
helpWidget view = [whamlet|
$maybe err <- fvErrors view
  <div .invalid-feedback style="display: block;">
    #{err}
$maybe tt <- fvTooltip view
  <small .form-text .text-muted>
    #{tt}
|]

-- | How the 'bootstrapSubmit' button should be rendered.
data BootstrapSubmit msg =
  BootstrapSubmit
  { bsValue   :: msg -- ^ The text of the submit button.
  , bsClasses :: Text -- ^ Classes added to the @\<button>@.
  , bsAttrs   :: [(Text, Text)] -- ^ Attributes added to the @\<button>@.
  } deriving (Eq, Ord, Show, Read)

instance IsString msg => IsString (BootstrapSubmit msg) where
  fromString msg = BootstrapSubmit (fromString msg) "btn-primary" []

-- | A Bootstrap v4 submit button disguised as a field for
-- convenience.  For example, if your form currently is:
--
-- > Person <$> areq textField "Name"  Nothing
-- >    <*> areq textField "Surname" Nothing
--
-- Then just change it to:
--
-- > Person <$> areq textField "Name"  Nothing
-- >    <*> areq textField "Surname" Nothing
-- >    <*  bootstrapSubmit ("Register" :: BootstrapSubmit Text)
--
-- (Note that '<*' is not a typo.)
--
-- Alternatively, you may also just create the submit button
-- manually as well in order to have more control over its
-- layout.
bootstrapSubmit :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m) =>
  BootstrapSubmit msg -> AForm m ()
bootstrapSubmit = formToAForm . fmap (second return) . mbootstrapSubmit

-- | Same as 'bootstrapSubmit' but for monadic forms.  This isn't
-- as useful since you're not going to use 'renderBootstrap4'
-- anyway.
mbootstrapSubmit :: (RenderMessage site msg, HandlerSite m ~ site, MonadHandler m) =>
  BootstrapSubmit msg -> MForm m (FormResult (), FieldView site)
mbootstrapSubmit (BootstrapSubmit msg classes attrs) =
  let res = FormSuccess ()
      widget = [whamlet|<button class="btn #{classes}" type=submit *{attrs}>_{msg}|]
      fv  = FieldView
            { fvLabel    = ""
            , fvTooltip  = Nothing
            , fvId       = bootstrapSubmitId
            , fvInput    = widget
            , fvErrors   = Nothing
            , fvRequired = False
            }
  in return (res, fv)

-- | A royal hack.  Magic id used to identify whether a field
-- should have no label.  A valid HTML4 id which is probably not
-- going to clash with any other id should someone use
-- 'bootstrapSubmit' outside 'renderBootstrap4'.
bootstrapSubmitId :: Text
bootstrapSubmitId = "b:ootstrap___unique__:::::::::::::::::submit-id"


-- | Creates an input with @type="radio"@ for selecting one option.
-- base on source code of radioField from Yesod.Form
radioFieldBs4 :: (Eq a, RenderMessage site FormMessage)
              => HandlerOf site (OptionList a)
              -> Field (HandlerOf site) a
radioFieldBs4 = selectFieldHelper
    (\ _theId _name _attrs inside -> [whamlet|
$newline never
^{inside}
|])
    (\theId name isSel -> [whamlet|
$newline never
<div .form-check>
  <input id=#{theId}-none type=radio name=#{name} value=none :isSel:checked .form-check-input>
  <label .radio for=#{theId}-none .form-check-label>
    _{MsgSelectNone}
|])
    (\theId name attrs value isSel text -> [whamlet|
$newline never
<div .form-check>
  <input id=#{theId}-#{value} type=radio name=#{name} value=#{value} :isSel:checked *{attrs} .form-check-input>
  <label .radio for=#{theId}-#{value} .form-check-label>
    #{text}
|])

-- | Creates an input with @type="radio"@ for selecting one option.
radioFieldListBs4 :: (Eq a, RenderMessage site FormMessage, RenderMessage site msg)
                  => [(msg, a)]
                  -> Field (HandlerOf site) a
radioFieldListBs4 = radioFieldBs4 . optionsPairs


-- | Creates a group of radio buttons to answer the question given in the message. Radio buttons are used to allow differentiating between an empty response (@Nothing@) and a no response (@Just False@). Consider using the simpler 'checkBoxField' if you don't need to make this distinction.
--
-- If this field is optional, the first radio button is labeled "\<None>", the second \"Yes" and the third \"No".
--
-- If this field is required, the first radio button is labeled \"Yes" and the second \"No".
--
-- (Exact label titles will depend on localization).
boolFieldBs4 :: Monad m => RenderMessage (HandlerSite m) FormMessage => Field m Bool
boolFieldBs4 = Field
      { fieldParse = \e _ -> return $ boolParser e
      , fieldView = \theId name attrs val isReq -> do
        [whamlet|
$newline never
  $if not isReq
    <div .form-check>
      <input id=#{theId}-none *{attrs} type=radio name=#{name} value=none checked .form-check-input>
      <label for=#{theId}-none>_{MsgSelectNone}


<div .form-check>
  <input id=#{theId}-yes *{attrs} type=radio name=#{name} value=yes :showVal id val:checked .form-check-input>
  <label for=#{theId}-yes .form-check-label>_{MsgBoolYes}

<div .form-check>
  <input id=#{theId}-no *{attrs} type=radio name=#{name} value=no :showVal not val:checked .form-check-input>
  <label for=#{theId}-no .form-check-label>_{MsgBoolNo}
|]
    , fieldEnctype = UrlEncoded
    }
  where
    boolParser [] = Right Nothing
    boolParser (x:_) = case x of
      "" -> Right Nothing
      "none" -> Right Nothing
      "yes" -> Right $ Just True
      "on" -> Right $ Just True
      "no" -> Right $ Just False
      "true" -> Right $ Just True
      "false" -> Right $ Just False
      t -> Left $ SomeMessage $ MsgInvalidBool t
    showVal = either (\_ -> False)


-- | Creates an input with @type="checkbox"@.
--   While the default @'boolField'@ implements a radio button so you
--   can differentiate between an empty response (@Nothing@) and a no
--   response (@Just False@), this simpler checkbox field returns an empty
--   response as @Just False@.
--
--   Note that this makes the field always optional.
--
checkBoxFieldBs4 :: Monad m => Field m Bool
checkBoxFieldBs4 = Field
    { fieldParse = \e _ -> return $ checkBoxParser e
    , fieldView  = \theId name attrs val _ -> [whamlet|
$newline never
<input id=#{theId} *{attrs} type=checkbox name=#{name} value=yes :showVal id val:checked .form-check-input>
|]
    , fieldEnctype = UrlEncoded
    }

    where
        checkBoxParser [] = Right $ Just False
        checkBoxParser (x:_) = case x of
            "yes" -> Right $ Just True
            "on" -> Right $ Just True
            _     -> Right $ Just False

        showVal = either (\_ -> False)


checkboxesFieldListBs4 :: (Eq a, RenderMessage site msg)
                       => [(msg, a)]
                       -> Field (HandlerOf site) [a]
checkboxesFieldListBs4 = checkboxesFieldBs4 . optionsPairs

-- | Creates an input with @type="checkbox"@ for selecting multiple options.
checkboxesFieldBs4 :: Eq a
                   => HandlerOf site (OptionList a)
                   -> Field (HandlerOf site) [a]
checkboxesFieldBs4 ioptlist = (multiSelectField ioptlist)
    { fieldView =
        \theId name attrs val _isReq -> do
            opts <- fmap olOptions $ handlerToWidget ioptlist
            let optselected (Left _) _ = False
                optselected (Right vals) opt = (optionInternalValue opt) `elem` vals
            [whamlet|
                $forall (idx, opt) <- zip_index opts
                  $with ident <- mk_opt_ident theId idx
                    <div .form-check>
                      <input .form-check-input type=checkbox name=#{name} value=#{optionExternalValue opt} ##{ident} *{attrs} :optselected val opt:checked>
                      <label .form-check-label for=#{ident}>#{optionDisplay opt}
                |]
    }
    where zip_index = zip ([1..] :: [Int])
          mk_opt_ident theId idx = theId <> "-" <> tshow idx

#if !MIN_VERSION_yesod_form(1, 6, 0)
-- | Copied from source of Yesod.Form.Types.
-- change signatures a little to make it compatible
selectFieldHelper
        :: (Eq a, RenderMessage site FormMessage)
        => (Text -> Text -> [(Text, Text)] -> WidgetOf site -> WidgetOf site)
        -> (Text -> Text -> Bool -> WidgetOf site)
        -> (Text -> Text -> [(Text, Text)] -> Text -> Bool -> Text -> WidgetOf site)
        -> HandlerOf site (OptionList a)
        -> Field (HandlerOf site) a
selectFieldHelper outside onOpt inside opts' = Field
    { fieldParse = \x _ -> do
        opts <- opts'
        return $ selectParser opts x
    , fieldView = \theId name attrs val isReq -> do
        opts <- fmap olOptions $ handlerToWidget opts'
        outside theId name attrs $ do
            unless isReq $ onOpt theId name $ not $ render opts val `elem` map optionExternalValue opts
            flip mapM_ opts $ \opt -> inside
                theId
                name
                ((if isReq then (("required", "required"):) else id) attrs)
                (optionExternalValue opt)
                ((render opts val) == optionExternalValue opt)
                (optionDisplay opt)
    , fieldEnctype = UrlEncoded
    }
  where
    render _ (Left _) = ""
    render opts (Right a) = maybe "" optionExternalValue $ listToMaybe $ filter ((== a) . optionInternalValue) opts
    selectParser _ [] = Right Nothing
    selectParser opts (s:_) = case s of
            "" -> Right Nothing
            "none" -> Right Nothing
            x -> case olReadExternal opts x of
                    Nothing -> Left $ SomeMessage $ MsgInvalidEntry x
                    Just y -> Right $ Just y
#endif
