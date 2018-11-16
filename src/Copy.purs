module Copy where

import Data.Maybe (Maybe(..))
import Optic.Core (set)
import Prelude ((#), ($))
import Types (Link, _title, defaultLink)



data CopyElem
  = CpStr String
  | CpLink Link

type Copy = Array CopyElem

copy ::
  { svgCredits :: Array CopyElem
  }
copy =
  { svgCredits :
    [ CpStr "Icons made by "
    , CpLink
        $ defaultLink { href : links.freepik, text : "Freepik" }
    , CpStr " from "
    , CpLink
        $ defaultLink { href : links.flaticon, text : "www.flaticon.com" }
        # set _title (Just "Flaticon")
    , CpStr " are licensed by "
    , CpLink
        $ defaultLink { href : links.creativeCommons, text : "CC 3.0 BY" }
        # set _title (Just "Creative Commons BY 3.0")
    ]
  }

links ::
  { freepik :: String
  , flaticon :: String
  , creativeCommons :: String
  }
links =
  { freepik : "https://www.freepik.com"
  , flaticon : "https://www.flaticon.com"
  , creativeCommons: "http://creativecommons.org/licenses/by/3.0/"
  }
