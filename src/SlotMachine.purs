module SlotMachine where

import Prelude

import Assets (File(File), Svg, assets_svg)
import CSS (CSS)
import CSS as CSS
import CSS.Animation (AnimationName(..), animation, forwards, iterationCount, normalAnimationDirection)
import CSS.Background (angular, backgroundColor, backgroundImage)
import CSS.Border (border, borderRadius, solid)
import CSS.Box (borderBox, boxSizing)
import CSS.Color (Color, rgb, rgba)
import CSS.Common as CMN
import CSS.Display (absolute, block, display, grid, inlineBlock, position, relative, flex)
import CSS.Flexbox (alignItems, column, flexDirection, flexGrow, flexShrink, justifyContent, row)
import CSS.Font (bold, color, fontSize, fontWeight)
import CSS.FontFace (fontFaceFamily)
import CSS.Geometry (height, lineHeight, margin, marginBottom, maxWidth, padding, paddingTop, width)
import CSS.Gradient (linearGradient)
import CSS.Overflow (hidden, overflow, overflowAuto)
import CSS.Pseudo (hover)
import CSS.Selector (element, with)
import CSS.Size (Size, deg, pct, px, vh)
import CSS.String (fromString)
import CSS.Stylesheet (keyframes, select)
import CSS.Text (letterSpacing)
import CSS.TextAlign (textAlign, center)
import CSS.Time (Time, sec)
import CSS.Transition (TimingFunction(..), easeOut)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM, randomInt)
import Copy (Copy, CopyElem(CpLink, CpStr), copy)
import Data.Array (concat, drop, filter, foldr, length, replicate, take)
import Data.Enum (enumFromTo)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Monoid (mempty)
import Data.NonEmpty ((:|))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Data.Typelevel.Num (D3)
import Data.Vec (Vec, empty, toArray, (+>))
import Debug.Trace (spy)
import Halogen (ClassName(ClassName), IProp, liftEff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events (input_)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Types (Link)


moduleName :: String
moduleName = "SlotMachine"

type State =
  { reels :: Vec D3
      { from :: Int
      , to :: Int
      , extraSpins :: Int
      , randInt :: Int
      }
  }

data Query a
  = Roll a
  | Initialize a

type IO eff =
  Aff (random :: RANDOM | eff)

--------------------------------------------------------------------------------

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

render :: State -> H.ComponentHTML Query
render state =
  viewMain state Nothing

initialState :: State
initialState =
  { reels }
  where
    reels =
      map
        (\i ->
          { from : i
          , to: i
          , extraSpins : 0
          , randInt : i
          }
        )
        (0 +> 1 +> 2 +> empty)

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (IO eff)
eval = case _ of
  Roll next -> do
    state <- H.get

    let n = length config.symbols

    reels <- state.reels
      # traverse
        (\old -> do
          to <- randomInt 0 (n-1)
          extraSpins <- randomInt 1 4
          randInt <- randomInt 0 100000
          pure
            { from : old.to
            , to
            , extraSpins
            , randInt
            }
        )
      # liftEff

    H.put $ state
      { reels = reels
      }

    pure next

  Initialize next -> do
    pure next

--------------------------------------------------------------------------------

config ::
  { symbols :: Array (File Svg)
  }
config =
  { symbols : enumFromTo bottom top # map assets_svg # take 5
  }

colors ::
  { link :: Color
  , dark :: Color
  , lightGrey :: Color
  , lightYellow :: Color
  , cta :: Color
  }
colors =
  { link : rgb 175 123 124
  , dark : rgb 53 14 15
  , lightGrey : rgb 240 240 240
  , lightYellow : rgb 240 240 173
  , cta : rgb 255 181 181
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
      mixinFullSize

    styleSlot = do
      marginBottom $ vh 7.0
      flexShrink 0
      justifyContent CMN.center

--------------------------------------------------------------------------------

viewSlot :: State -> Maybe CSS -> H.ComponentHTML Query
viewSlot { reels } outStyle =
  HH.div
    [ nameIt "viewSlot"
    , combineStyles style outStyle
    ]
    ( map
        (\{ from, to, extraSpins, randInt } ->
           viewReel
             { from
             , to
             , extraSpins
             , occurences : occurences to (map _.to reelsArray)
             , maxSpinPathLength
             , randInt
             }
             Nothing
        )
        reelsArray
    )
  where
    reelsArray = toArray reels

    maxSpinPathLength =
      map
        (\{from, to, extraSpins} ->
          getSpinPath from to extraSpins reelsArray
          # length
        )
        reelsArray
      # foldr max 0

    style = do
      display flex
      flexDirection row
      maxWidth $ px 600.0
      width $ pct 100.0

--------------------------------------------------------------------------------

viewReel ::
  { from :: Int
  , to :: Int
  , extraSpins :: Int
  , occurences :: Int
  , maxSpinPathLength :: Int
  , randInt :: Int
  }
  -> Maybe CSS
  -> H.ComponentHTML Query
viewReel { from, to, extraSpins, occurences, maxSpinPathLength, randInt } outStyle =
  viewBoxAspect
    { aspect : 1.0 }
    (Just style)
    ( HH.div
        [ HCSS.style styleWrapper ]
        [ HH.div
            [ HCSS.style styleReel ]
            (map viewReelItem spinPath <> [ HCSS.stylesheet sheetReel ])
        , HH.div [HCSS.style styleBlend] [  ]
        , HCSS.stylesheet sheet
        ]
    )
  where
    styleWrapper = do
      mixinFullSize

    spinPath = config.symbols
      # getSpinPath from to extraSpins

    animName = "anim" <> show randInt

    style = do
      overflow hidden
      backgroundColor colors.lightGrey
      borderRadius (pct 15.0) (pct 15.0) (pct 15.0) (pct 15.0)
      margin (pct 2.0) (pct 2.0) (pct 2.0) (pct 2.0)
      if occurences > 1
         then mixinDefaultAnimation
           { name : animName
           , timingFunction : easeInOut
           , duration : sec 1.0
           , delay : maxSpinDuration
           }
         else pure mempty

    sheet = do
      keyframes
        animName
        ( (0.0 /\ do
             prop "transform" $ "rotateY(" <> show 0.0 <> "deg)"
          ) :|
          [ (100.0 /\ do
               prop "transform" $ "rotateY(" <> show 360.0 <> "deg)"
               backgroundColor colors.lightYellow
            )
          ]
        )

    styleBlend = do
      let transparent = rgba zero zero zero zero
      backgroundImage $ linearGradient (angular $ deg zero)
        [ (colors.lightGrey /\ pct 0.0)
        , (transparent /\ pct 5.0)
        , (transparent /\ pct 90.0)
        , (colors.lightGrey /\ pct 100.0)
        ]
      position absolute
      mixinStickToEdges

    animNameReel = "animReel" <> show randInt

    maxSpinDuration = sec $ (toNumber maxSpinPathLength * 0.3) + (spy 2.0)

    spinDuration = sec $ toNumber (length spinPath) * 0.3

    styleReel = do
      boxSizing borderBox
      mixinFullSize
      display flex
      flexDirection column

      mixinDefaultAnimation
        { name : animNameReel
        , timingFunction : easeOut
        , duration : spinDuration
        , delay : sec zero
        }

    sheetReel = do
      let
        pctY = -100.0 * toNumber (length spinPath - 1)
      keyframes
        animNameReel
        ( (0.0 /\ do
             mixinTransformTranslateY 0.0
          ) :|
          [ (90.0 /\ do
               mixinTransformTranslateY $ pctY - 30.0
            )
          , (100.0 /\ do
               mixinTransformTranslateY pctY
            )
          ]
        )

--------------------------------------------------------------------------------

viewReelItem :: File Svg -> H.ComponentHTML Query
viewReelItem sym =
  viewBoxAspect
    { aspect : 1.0 }
    (Just style)
    (viewSvg sym (Just styleImg))
  where
    style = do
      width (pct 100.0)
      height (pct 100.0)

    styleImg =  do
      flexGrow 0
      flexShrink 0
      margin (pct 10.0) (pct 10.0) (pct 10.0) (pct 10.0)

--------------------------------------------------------------------------------

viewButton ::
  { onClick :: Unit -> Query Unit }
  -> Maybe CSS
  -> H.ComponentHTML Query
viewButton { onClick } outStyle =
  HH.button
    [ nameIt name
    , combineStyles style outStyle
    , HE.onClick (input_ onClick)
    ]
    [ HH.text "GO"
    , HCSS.stylesheet sheet
    ]
  where
    name = "viewButton"

    sheet = do
      select (with (element $ getClassStr name) hover) do
        backgroundColor colors.lightGrey
      select (element $ getClassStr name) do
        backgroundColor colors.cta
      select (element $ getClassStr name <> ":focus") do
        prop "outline" "0"

    style = do
      width $ px 200.0
      height $ px 40.0
      prop "cursor" "pointer"
      fontFaceFamily "Arial"
      fontSize $ px 20.0
      color colors.dark
      fontWeight bold
      mixinBorderRadius (px 15.0)
      letterSpacing (px 5.0)
      border solid (px 3.0) colors.lightYellow



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
      mixinStickToEdges

--------------------------------------------------------------------------------

nameIt :: forall a b. String -> IProp ( "class" :: String | b ) a
nameIt str =
  HP.class_ (ClassName $ moduleName <> "_" <> str)

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

occurences :: forall a. Eq a => a -> Array a -> Int
occurences x xs =
  filter (_ == x) xs # length

--------------------------------------------------------------------------------

mixinTransformTranslateY :: Number -> CSS
mixinTransformTranslateY n =
  prop "transform" $ "translateY(" <> show n <> "%)"

mixinDefaultAnimation ::
  { timingFunction :: TimingFunction
  , duration :: Time
  , delay :: Time
  , name :: String
  }
  -> CSS
mixinDefaultAnimation { name, duration, timingFunction, delay } =
  animation
    (AnimationName $ fromString name)
    duration
    timingFunction
    delay
    (iterationCount 1.0)
    normalAnimationDirection
    forwards

mixinStickToEdges :: CSS
mixinStickToEdges = do
  CSS.top $ px zero
  CSS.left $ px zero
  CSS.right $ px zero
  CSS.bottom $ px zero

mixinFullSize :: CSS
mixinFullSize = do
  width (pct 100.0)
  height (pct 100.0)

easeInOut :: TimingFunction
easeInOut = TimingFunction $ fromString "ease-in-out"

mixinBorderRadius :: forall a. Size a -> CSS
mixinBorderRadius s = do
  borderRadius s s s s
