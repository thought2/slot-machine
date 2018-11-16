module Types where

import Data.Maybe (Maybe(..))
import Optic.Lens (lens)
import Optic.Types (Lens)


type LinkReq =
  ( text :: String
  , href :: String
  )

type LinkOpt =
  ( title :: Maybe String
  , target :: Maybe String
  | LinkReq
  )

type Link = Record LinkOpt

defaultLink :: Record LinkReq -> Record LinkOpt
defaultLink { href, text } =
  { href
  , text
  , title : Nothing
  , target : Nothing
  }

_title :: forall b r a t s. Lens { title :: a | r } { title :: b | r } a b
_title = lens _.title (\o x -> o { title = x })

_href :: forall b r a t s. Lens { href :: a | r } { href :: b | r } a b
_href = lens _.href (\o x -> o { href = x })

_text :: forall b r a t s. Lens { text :: a | r } { text :: b | r } a b
_text = lens _.text (\o x -> o { text = x })

_target :: forall b r a t s. Lens { target :: a | r } { target :: b | r } a b
_target = lens _.target (\o x -> o { target = x })
