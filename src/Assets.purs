module Assets where

import Data.Bounded (class Bounded)
import Data.Enum (class Enum)
import Data.Eq (class Eq)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Bounded (genericBottom, genericTop)
import Data.Generic.Rep.Enum (genericPred, genericSucc)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.Ord (class Ord)


data Svg
data Mp3

data File ft = File String

data Assets
  = Assets_Svg_ Assets_Svg
  | Assets_Mp3_ Assets_Mp3

data Assets_Svg
  = Assets_Svg_Hamburger
  | Assets_Svg_Steak
  | Assets_Svg_Orange
  | Assets_Svg_Lettuce
  | Assets_Svg_Fish
  | Assets_Svg_Chicken
  | Assets_Svg_Banana
  | Assets_Svg_OliveOil
  | Assets_Svg_Chicken1
  | Assets_Svg_Noodles
  | Assets_Svg_Apple
  | Assets_Svg_Shrimp
  | Assets_Svg_Soup
  | Assets_Svg_Strawberry
  | Assets_Svg_Pineapple
  | Assets_Svg_Spaghetti
  | Assets_Svg_FrenchFries
  | Assets_Svg_Broccoli
  | Assets_Svg_Tomato
  | Assets_Svg_Pizza

data Assets_Mp3
  = Assets_Mp3_Win
  | Assets_Mp3_Lose


derive instance genericAssets :: Generic Assets _

derive instance genericAssets_Svg :: Generic Assets_Svg _

derive instance genericAssets_Mp3 :: Generic Assets_Mp3 _


instance eqAssets :: Eq Assets where
  eq = genericEq

instance eqAssets_Svg :: Eq Assets_Svg where
  eq = genericEq

instance eqAssets_Mp3 :: Eq Assets_Mp3 where
  eq = genericEq


instance ordAssets :: Ord Assets where
  compare = genericCompare

instance ordAssets_Svg :: Ord Assets_Svg where
  compare = genericCompare

instance ordAssets_Mp3 :: Ord Assets_Mp3 where
  compare = genericCompare


instance boundedAssets :: Bounded Assets where
  top = genericTop
  bottom = genericBottom

instance boundedAssets_Svg :: Bounded Assets_Svg where
  top = genericTop
  bottom = genericBottom

instance boundedAssets_Mp3 :: Bounded Assets_Mp3 where
  top = genericTop
  bottom = genericBottom


instance enumAssets :: Enum Assets where
  succ = genericSucc
  pred = genericPred

instance enumAssets_Svg :: Enum Assets_Svg where
  succ = genericSucc
  pred = genericPred

instance enumAssets_Mp3 :: Enum Assets_Mp3 where
  succ = genericSucc
  pred = genericPred


assets_svg :: Assets_Svg -> File Svg
assets_svg f =
  case f of
    Assets_Svg_Hamburger -> File "assets/svg/001-hamburger.svg"
    Assets_Svg_Steak -> File "assets/svg/002-steak.svg"
    Assets_Svg_Orange -> File "assets/svg/003-orange.svg"
    Assets_Svg_Lettuce -> File "assets/svg/004-lettuce.svg"
    Assets_Svg_Fish-> File "assets/svg/005-fish.svg"
    Assets_Svg_Chicken -> File "assets/svg/006-chicken.svg"
    Assets_Svg_Banana -> File "assets/svg/007-banana.svg"
    Assets_Svg_OliveOil -> File "assets/svg/008-olive-oil.svg"
    Assets_Svg_Chicken1 -> File "assets/svg/009-chicken-1.svg"
    Assets_Svg_Noodles -> File "assets/svg/010-noodles.svg"
    Assets_Svg_Apple -> File "assets/svg/011-apple.svg"
    Assets_Svg_Shrimp -> File "assets/svg/012-shrimp.svg"
    Assets_Svg_Soup -> File "assets/svg/013-soup.svg"
    Assets_Svg_Strawberry -> File "assets/svg/014-strawberry.svg"
    Assets_Svg_Pineapple -> File "assets/svg/015-pineapple.svg"
    Assets_Svg_Spaghetti -> File "assets/svg/016-spaghetti.svg"
    Assets_Svg_FrenchFries -> File "assets/svg/017-french-fries.svg"
    Assets_Svg_Broccoli -> File "assets/svg/018-broccoli.svg"
    Assets_Svg_Tomato -> File "assets/svg/019-tomato.svg"
    Assets_Svg_Pizza -> File "assets/svg/020-pizza.svg"

assets_mp3 :: Assets_Mp3 -> File Mp3
assets_mp3 f =
  case f of
    Assets_Mp3_Win -> File "assets/mp3/win.mp3"
    Assets_Mp3_Lose -> File "assets/mp3/lose.mp3"
