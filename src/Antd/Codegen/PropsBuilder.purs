module Antd.Codegen.PropsBuilder
       ( PropsBuilder
       , buildProps
       , addProp
       ) where

import Prelude

import Antd.Codegen.Types (Prop, Typ)
import Control.Monad.State (State, execState, modify_)
import Data.Array as Array
import Data.List (List)
import Data.List as List
import Data.Maybe (Maybe(..))

type PropsBuilder a = State (List Prop) a

buildProps :: PropsBuilder Unit -> Array Prop
buildProps =
  Array.fromFoldable <<< List.reverse <<< (flip execState) mempty

addProp :: String -> Typ -> PropsBuilder Unit
addProp name typ =
  modify_ (List.Cons { name
                     , doc: Nothing
                     , typ
                     })
