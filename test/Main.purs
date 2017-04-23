module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Optic (ctor)
import Type.Data.Symbol (SProxy(..))
import Data.Lens (over, view, preview)
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)

data I = I String
derive instance genericI :: Generic I _
derive instance eqI :: Eq I
_I = SProxy :: SProxy "I"

data E = L Int | R Boolean
derive instance genericE :: Generic E _
derive instance eqE :: Eq E
_L = SProxy :: SProxy "L"
_R = SProxy :: SProxy "R"

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "I _I" *> do
    assert $ I "Hi!" == over (ctor _I) (_ <> "!") (I "Hi")
    assert $ "Hi" == view (ctor _I) (I "Hi")

  log "E _L" *> do
    let f = over (ctor _L) (10 * _)
    assert $ L 420  == f (L 42)
    assert $ R true == f (R true)
    assert $ Just 42 == preview (ctor _L) (L 42)
    assert $ Nothing == preview (ctor _L) (R true)

  log "E _R" *> do
    let f = over (ctor _R) not
    assert $ L 42    == f (L 42)
    assert $ R false == f (R true)
    assert $ Nothing   == preview (ctor _R) (L 42)
    assert $ Just true == preview (ctor _R) (R true)

  log "All done!"
