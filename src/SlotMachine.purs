module SlotMachine where

import Prelude

import Assets (File(File), Svg, assets_svg)
import CSS (AnimationName(..), CSS, Color, TimingFunction(..), absolute, alignItems, animation, backgroundColor, backwards, block, borderBox, boxSizing, color, column, display, easeOut, flex, flexDirection, flexGrow, flexShrink, fontFaceFamily, fontSize, forwards, fromString, green, grid, height, hover, infinite, inlineBlock, iterationCount, justifyContent, keyframes, lineHeight, linear, margin, marginBottom, maxWidth, normalAnimationDirection, padding, paddingTop, pct, position, px, red, relative, rgb, row, sec, vh, width, with, yellow)
import CSS as CSS
import CSS.Common as CMN
import CSS.Overflow (hidden, overflow, overflowAuto)
import CSS.Selector (element)
import CSS.Stylesheet (select)
import CSS.TextAlign (textAlign, center)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Control.Monad.List.Trans (repeat)
import Copy (Copy, CopyElem(CpLink, CpStr), copy)
import Data.Array (concat, drop, length, range, replicate, snoc, take, (:))
import Data.Enum (enumFromTo)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.NonEmpty (NonEmpty(..), (:|))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D3)
import Data.Vec (Vec, empty, toArray, (+>))
import Data.Vec as Vec
import Debug.Trace (spy)
import Halogen (ClassName(ClassName), IProp, liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events (input_)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Link)

type State =
  { reels :: Vec D3 { from :: Int, to :: Int }
  , extraSpins :: Int
  }

data Query a
  = Roll a

type IO eff =
  (Aff (random :: RANDOM | eff))

ui :: forall eff. H.Component HH.HTML Query Unit Void (IO eff)
ui =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer : Nothing
    , finalizer : Nothing
    }
  where

  initialState :: State
  initialState =
    { extraSpins : 0
    , reels : 1 +> 2 +> 3 +> empty
      # map (\i -> { from : i, to: i})
    }

  render :: State -> H.ComponentHTML Query
  render state =
    viewMain state Nothing

  eval :: Query ~> H.ComponentDSL State Query Void (IO eff)
  eval = case _ of
    Roll next -> do
      state <- H.get

      let n = length config.symbols

      randVec :: Vec D3 Int <- pure (randomInt 0 (n-1))
        # traverse id
        # liftEff

      let reels = Vec.zipWith
           (\to' {to} -> { from : to, to: to' })
           randVec
           state.reels

      H.put $ spy (state
        { extraSpins = 1
        , reels = reels
        })
      pure next

--------------------------------------------------------------------------------

config ::
  { symbols :: Array (File Svg)
  }
config =
  { symbols : enumFromTo bottom top # map assets_svg
  }

--------------------------------------------------------------------------------

viewMain :: State -> Maybe CSS -> H.ComponentHTML Query
viewMain state outStyle =
  HH.div
    [ nameIt "viewMain"
    , combineStyles style outStyle
    ]
    [ viewBody state (Just styleBody)
    , viewFooter Nothing
    ]
  where
    style = do
      prop "grid-gap" "20px"
      prop "grid-template-rows" "1fr auto"
      prop "grid-gap" "20px"
      display grid
      height $ pct 100.0
      boxSizing borderBox
      padding (px 27.0) (px 20.0) (px 10.0) (px 20.0)

    styleBody = do
      overflow overflowAuto

--------------------------------------------------------------------------------

viewFooter :: Maybe CSS -> H.ComponentHTML Query
viewFooter outStyle =
  viewCopy copy.svgCredits outStyle

--------------------------------------------------------------------------------

viewBody :: State -> Maybe CSS -> H.ComponentHTML Query
viewBody state outStyle =
  HH.div
    [ nameIt "viewBody"
    , combineStyles style outStyle
    ]
    [ viewSlot state (Just styleSlot)
    , viewButton { onClick : Roll } Nothing
    ]
  where
    style = do
      display flex
      flexDirection column
      alignItems CMN.center
      justifyContent CMN.center
      width $ pct 100.0
      height $ pct 100.0

    styleSlot = do
      marginBottom $ vh 7.0
      flexShrink 0
      justifyContent CMN.center

--------------------------------------------------------------------------------

viewSlot :: State -> Maybe CSS -> H.ComponentHTML Query
viewSlot { reels, extraSpins } outStyle =
  HH.div
    [ nameIt "viewSlot"
    , combineStyles style outStyle
    ]
    ( map
        (\{ from, to } -> viewReel { from, to, extraSpins } Nothing)
        (toArray reels)
    )
  where
    style = do
      display flex
      flexDirection row
      maxWidth $ px 600.0
      width $ pct 100.0

--------------------------------------------------------------------------------

viewReel ::
  { from :: Int, to :: Int, extraSpins :: Int }
  -> Maybe CSS
  -> H.ComponentHTML Query
viewReel { from, to, extraSpins } outStyle =
  viewBoxAspect
    { aspect : 1.0 }
    (Just style)
    (HH.div
       [ HCSS.style styleReel ]
       ( map
           (\sym -> viewSvg sym (Just styleItem))
           spinPath
         # \xs -> snoc xs (HCSS.stylesheet sheet)
       )
    )
  where
    spinPath = config.symbols
      # getSpinPath from to extraSpins

    style = do
      overflow hidden

    animName = "anim" <> show from <> show to

    styleReel = do
      boxSizing borderBox
      width (pct 100.0)
      height (pct 100.0)
      display flex
      flexDirection column

    styleItem = do
      flexGrow 0
      flexShrink 0
      margin (pct 5.0) (pct 5.0) (pct 5.0) (pct 5.0)
      animation
        (AnimationName $ fromString animName)
        (sec $ toNumber (length spinPath) * 0.1)
        (TimingFunction $ fromString "ease-in-out")
        (sec 0.0)
        (iterationCount 1.0)
        normalAnimationDirection
        backwards

    sheet = do
      let pctY = -100.0 * toNumber (length spinPath - 1)
          transformTranslateY n =
            prop "transform" $ "translateY(" <> show n <> "%)"
      keyframes
        animName
        ( (0.0 /\ do transformTranslateY pctY) :|
          [ (80.0 /\ do transformTranslateY 20.0)
          , (100.0 /\ do transformTranslateY 0.0)
          ]
        )



viewReel' :: { index :: Int } -> Maybe CSS -> H.ComponentHTML Query
viewReel' { index } outStyle =
  viewBoxAspect
    { aspect : 1.0 }
    Nothing
    ( HH.div
        [HCSS.style styleReel]
        [HH.text $ show index]
    )
  where
    styleReel = do
      boxSizing borderBox
      width (pct 100.0)
      height (pct 100.0)

--------------------------------------------------------------------------------

viewButton :: { onClick :: Unit -> Query Unit } -> Maybe CSS -> H.ComponentHTML Query
viewButton { onClick } outStyle =
  HH.div
    [ nameIt name
    , combineStyles style outStyle
    , HE.onClick (input_ onClick)
    ]
    [ HH.text "AAA"
    , HCSS.stylesheet sheet
    ]
  where
    name = "viewButton"

    sheet = select (with (element $ getClassStr name) hover) do
      backgroundColor red

    style = do
      maxWidth $ px 240.0
      height $ px 40.0
      prop "cursor" "pointer"

--------------------------------------------------------------------------------

viewSvg :: File Svg -> Maybe CSS -> H.ComponentHTML Query
viewSvg (File path) outStyle =
  HH.img
    [ nameIt "viewSvg"
    , combineStyles style outStyle
    , HP.src path
    ]
  where
    style = do
      prop "zoom" "0"

--------------------------------------------------------------------------------

viewLink :: Link -> Maybe CSS -> H.ComponentHTML Query
viewLink { text, href, title, target } outStyle =
  HH.a
    ( [ nameIt "viewLink"
      , combineStyles style outStyle
      , HP.href href
      , HP.title (maybe text id title)
      ]
      <> maybe mempty (pure <<< HP.target) target
    )
    [ HH.text text ]
  where
    style = do
      color colors.link

--------------------------------------------------------------------------------

viewCopy :: Copy -> Maybe CSS -> H.ComponentHTML Query
viewCopy copy outStyle =
  HH.div
    [ nameIt "viewCopy", combineStyles style outStyle ]
    (map viewElem copy)
  where
    style = do
      fontFaceFamily "Arial"
      fontSize $ px 9.0
      color colors.dark
      textAlign center
      lineHeight $ px 12.0

    viewElem = case _ of
      CpStr str -> HH.text str
      CpLink link -> viewLink link Nothing

--------------------------------------------------------------------------------

viewBoxAspect ::
  { aspect :: Number }
  -> Maybe CSS
  -> H.ComponentHTML Query
  -> H.ComponentHTML Query
viewBoxAspect { aspect } outStyle children =
  HH.div
    [ nameIt "viewBoxAspect"
    , combineStyles style outStyle
    ]
    [ HH.div
      [ HCSS.style stylePlaceholder ]
      []
    , HH.div
      [ HCSS.style styleContent ]
      [ children ]
    ]
  where
    style = do
      position relative
      width $ pct 100.0
      height $ pct $ 100.0 / aspect
      display inlineBlock

    stylePlaceholder = do
      display block
      paddingTop $ pct $ 100.0 / aspect

    styleContent = do
      position absolute
      CSS.top $ px zero
      CSS.left $ px zero
      CSS.right $ px zero
      CSS.bottom $ px zero

--------------------------------------------------------------------------------

colors ::
  { link :: Color
  , dark :: Color
  }
colors =
  { link : rgb 175 123 124
  , dark : rgb 53 14 15
  }

nameIt :: forall a b. String -> IProp ( "class" :: String | b ) a
nameIt str =
  HP.class_ (ClassName $ moduleName <> "_" <> str)

moduleName :: String
moduleName = "SlotMachine"

combineStyles :: forall a b. CSS -> Maybe CSS -> IProp ( style :: String | a ) b
combineStyles style maybeStyle =
  HCSS.style $ maybe style ((<>) style) maybeStyle

getClassStr :: String -> String
getClassStr name =
  "." <> moduleName <> "_" <> name

prop :: String -> String -> CSS
prop key value =
  CSS.key (fromString key) value

getSpinPath :: forall a. Int -> Int -> Int -> Array a -> Array a
getSpinPath from to extraSpins xs =
  path <> extra
  where
    path =
      if to >= from then
        drop from xs
        # take (to + 1 - from)
      else
        drop from xs
        <> take (to + 1) xs

    extra = drop (to + 1) xs
      <> take (to + 1) xs
       # replicate extraSpins
       # concat
