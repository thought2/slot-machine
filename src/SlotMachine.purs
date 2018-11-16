module SlotMachine where

import CSS hiding (bottom,top,map)
import Prelude

import Assets (Assets_Svg(..), File(..), Svg, assets_svg)
import CSS as CSS
import CSS.Common (auto)
import CSS.Common as Common
import CSS.Overflow (hidden, overflow, overflowAuto)
import CSS.TextAlign (textAlign, center)
import Copy (Copy, CopyElem(CpLink, CpStr), copy)
import Data.Array (head)
import Data.Enum (enumFromTo)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Halogen (HTML)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Properties as HP
import Prelude as Prelude
import Types (Link)

type State = Boolean

type Config =
  { n :: Int
  }

data St
  = Rolling
  | Result (Array Int)

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

data Message = Toggled Boolean

component :: forall m. H.Component HH.HTML Query Unit Message m
component =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "O"
    in
      viewMain

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)


cls =
  { viewMain:
    { _main: H.ClassName "SlotMachine.viewMain"
    , body: H.ClassName "SlotMachine.viewMain.body"
    , footer: H.ClassName "SlotMachine.viewMain.footer"
    }
  , viewBody:
    { _main: H.ClassName "SlotMachine.viewBody fn"
    }
  }

viewMain :: forall p i. HTML p i
viewMain =
  HH.div [ HP.class_ cls.viewMain._main, CSS.style style ]
    [ HH.div [ HP.class_ cls.viewMain.body, CSS.style styleBody ]
      [ viewBody
      ]
    , HH.div [ HP.class_ cls.viewMain.footer ]
      [ viewFooter
      ]
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

styleMain =
  { _main: do
      key (fromString "grid-gap") "20px"
      key (fromString "grid-template-rows") "1fr auto"
      key (fromString "grid-gap") "20px"
      display grid
      height $ pct 100.0
      boxSizing borderBox
      padding (px 27.0) (px 20.0) (px 10.0) (px 20.0)
  , body: do
      overflow overflowAuto
  }


viewFooter = viewCopy copy.svgCredits

viewBody =
  HH.div [ HP.class_ cls.viewBody._main, CSS.style style ]
    [ HH.div [CSS.style styleSlot] [viewSlot]
    , viewButton
    ]
    where
      style = do
        display flex
        CSS.flexDirection column
        alignItems Common.center
        justifyContent Common.center
        width $ pct 100.0
        height $ pct 100.0

      styleSlot = do
        CSS.marginBottom $ vh 7.0
        width $ pct 100.0
        display flex
        flexShrink 0
        justifyContent Common.center

styleBody =
  { _main: do
      display flex
      CSS.flexDirection column
      alignItems Common.center
      justifyContent Common.center
      width $ pct 100.0
      height $ pct 100.0
  , slot: do
      CSS.marginBottom $ vh 7.0
      width $ pct 100.0
      display flex
      flexShrink 0
      justifyContent Common.center
  }

viewButton =
  HH.div
    [ CSS.style style ]
    [ HH.text "x" ]
    where
      style = do
        maxWidth $ px 240.0
        height $ px 40.0

viewSlot :: forall p i. HTML p i
viewSlot =
  HH.div
    [ CSS.style style ]
    [ viewBoxAspect 1.0 (HH.div [CSS.style tmp ] [HH.text "a"])
    , viewBoxAspect 1.0 (HH.div [CSS.style tmp ] [HH.text ""])
    , viewBoxAspect 1.0 (HH.div [CSS.style tmp ] [HH.text ""])
    ]
  where
    tmp = do
      boxSizing borderBox
      width (pct 100.0)
      height (pct 100.0)

    style = do
      display flex
      flexDirection row
      maxWidth $ px 600.0
      width $ pct 100.0
      flexShrink 0

viewSvg :: forall p i. Assets_Svg -> HTML p i
viewSvg file =
  HH.img
    [ HP.src path
    , CSS.style style
    ]
  where
    File path = assets_svg file
    style = do
      flexShrink 1
      flexGrow 1
--      width $ px 30.0
--      height $ px 30.0

viewVertical :: Int -> forall p i. HTML p i
viewVertical index =
  HH.div
      [ CSS.style stylePassepartout ]
      [ placeholder
      --, HH.div
        --[ CSS.style style]
        --[] -- (map viewSvg xs)
      ]
  where
    xs :: Array Assets_Svg
    xs = enumFromTo bottom top

    placeholder = maybe (HH.text "") viewSvg (head xs)

    stylePassepartout = do
      border solid (px 1.0) black
--      overflow hidden
      display flex
      flexShrink 1
      flexGrow 1

    style = do
      display flex
      flexDirection column
      transform (translate (px 0.0) (px (-30.0 * toNumber index)))

viewLink :: forall p i. Link -> HTML p i
viewLink { text, href, title, target } =
  HH.a
    (  [ HP.href href
       , HP.title (maybe text id title)
       , CSS.style style
       ]
    <> maybe mempty (pure <<< HP.target) target
    )
    [ HH.text text ]
    where
      style = do
        color colors.link

viewCopy :: forall p i. Copy -> HTML p i
viewCopy copy =
  HH.div
    [ CSS.style style
    ]
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
        CpLink link -> viewLink link

viewBoxAspect :: forall p i. Number -> HTML p i -> HTML p i
viewBoxAspect aspect children =
  HH.div
    [ CSS.style styleWrapper]
    [ HH.div
      [ CSS.style stylePlaceholder ]
      []
    , HH.div
      [ CSS.style styleContent ]
      [ children ]
    ]
  where
    styleWrapper = do
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

colors ::
  { link :: Color
  , dark :: Color
  }
colors =
  { link : rgb 175 123 124
  , dark : rgb 53 14 15
  }
