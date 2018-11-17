module SlotMachine where

import Prelude

import Assets (Assets(..), Assets_Svg, File(File), assets_svg)
import CSS (CSS, Color, absolute, alignItems, block, borderBox, boxSizing, color, column, display, flex, flexDirection, flexGrow, flexShrink, fontFaceFamily, fontSize, fromString, grid, height, inlineBlock, justifyContent, key, lineHeight, marginBottom, maxWidth, padding, paddingTop, pct, position, px, relative, rgb, row, vh, width)
import CSS (bottom, left, right, top) as CSS
import CSS.Common as CMN
import CSS.Overflow (overflow, overflowAuto)
import CSS.TextAlign (textAlign, center)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Copy (Copy, CopyElem(CpLink, CpStr), copy)
import Data.Array (length)
import Data.Enum (enumFromTo)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.Traversable (traverse)
import Data.Typelevel.Num (D3)
import Data.Vec (Vec, empty, toArray, (+>))
import Halogen (ClassName(ClassName), IProp, liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS (style) as CSS
import Halogen.HTML.Events (input_)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Link)


type State = Vec D3 Int

type Config =
  { n :: Int
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
  initialState = 1 +> 2 +> 3 +> empty

  render :: State -> H.ComponentHTML Query
  render state =
    viewMain state Nothing

  eval :: Query ~> H.ComponentDSL State Query Void (IO eff)
  eval = case _ of
    Roll next -> do
      state <- H.get
      let l = (enumFromTo bottom top :: Array Assets_Svg) # length
      randVec <- pure (randomInt 0 (l-1))
        # traverse id
        # liftEff
      H.put randVec
      pure next


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
      key (fromString "grid-gap") "20px"
      key (fromString "grid-template-rows") "1fr auto"
      key (fromString "grid-gap") "20px"
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
    , viewButton Nothing
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
viewSlot state outStyle =
  HH.div
    [ nameIt "viewSlot"
    , combineStyles style outStyle
    ]
    ( map
        (\index -> viewReel { index } Nothing)
        (toArray state)
    )
  where
    style = do
      display flex
      flexDirection row
      maxWidth $ px 600.0
      width $ pct 100.0

--------------------------------------------------------------------------------

viewReel :: { index :: Int } -> Maybe CSS -> H.ComponentHTML Query
viewReel { index } outStyle =
  viewBoxAspect
    { aspect : 1.0 }
    Nothing
    ( HH.div
        [CSS.style styleReel]
        [HH.text $ show index]
    )
  where
    styleReel = do
      boxSizing borderBox
      width (pct 100.0)
      height (pct 100.0)

--------------------------------------------------------------------------------

viewButton :: Maybe CSS -> H.ComponentHTML Query
viewButton outStyle =
  HH.div
    [ nameIt "viewButton"
    , combineStyles style outStyle
    , HE.onClick (input_ Roll)
    ]
    [ HH.text "AAA" ]
    where
      style = do
        maxWidth $ px 240.0
        height $ px 40.0

--------------------------------------------------------------------------------

viewSvg :: Assets_Svg -> Maybe CSS -> H.ComponentHTML Query
viewSvg file outStyle =
  HH.img
    [ nameIt "viewSvg"
    , combineStyles style outStyle
    , HP.src path
    ]
  where
    File path = assets_svg file
    style = do
      flexShrink 1
      flexGrow 1

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
      [ CSS.style stylePlaceholder ]
      []
    , HH.div
      [ CSS.style styleContent ]
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
  HP.class_ (ClassName $ "SlotMachine." <> str)

combineStyles :: forall a b. CSS -> Maybe CSS -> IProp ( style :: String | a ) b
combineStyles style maybeStyle =
  CSS.style $ maybe style ((<>) style) maybeStyle

--------------------------------------------------------------------------------

-- viewVertical :: Int -> forall p i. HTML p i
-- viewVertical index =
--   HH.div
--       [ CSS.style stylePassepartout ]
--       [ placeholder
--       --, HH.div
--         --[ CSS.style style]
--         --[] -- (map viewSvg xs)
--       ]
--   where
--     xs :: Array Assets_Svg
--     xs = enumFromTo bottom top

--     placeholder = maybe (HH.text "") (\x -> viewSvg x Nothing) (head xs)

--     stylePassepartout = do
--       border solid (px 1.0) black
-- --      overflow hidden
--       display flex
--       flexShrink 1
--       flexGrow 1

--     style = do
--       display flex
--       flexDirection column
--       transform (translate (px 0.0) (px (-30.0 * toNumber index)))
