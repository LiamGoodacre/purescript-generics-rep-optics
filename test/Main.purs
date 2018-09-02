module Test.Main where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Lens (Iso', Prism', over, preview)
import Data.Maybe (Maybe(..))
import Generic.Optic (noCtor, genericAbsurd, _Ctor')
import Test.Assert (assert)
import Effect (Effect)
import Effect.Console (log)
import Type.Data.Symbol (SProxy(..))

data VoidExample

derive instance genericVoidExammple :: Generic VoidExample _

voidExample :: Iso' VoidExample Void
voidExample = noCtor

absurdExample :: forall a . VoidExample -> a
absurdExample = genericAbsurd

data Example =
  A Int |
  B Boolean |
  C String

derive instance genericExample :: Generic Example _

derive instance eqExample :: Eq Example

_A :: Prism' Example Int
_A = _Ctor' (SProxy :: SProxy "A")

_B :: Prism' Example Boolean
_B = _Ctor' (SProxy :: SProxy "B")

_C :: Prism' Example String
_C = _Ctor' (SProxy :: SProxy "C")

main :: Effect Unit
main = do
  log "Example _A" *> do
    let f = over _A (10 * _)
    assert $ A 420 == f (A 42)
    assert $ B true == f (B true)
    assert $ C "Hi" == f (C "Hi")
    assert $ Just 42 == preview _A (A 42)
    assert $ Nothing == preview _A (B true)
    assert $ Nothing == preview _A (C "Hi")

  log "Example _B" *> do
    let f = over _B not
    assert $ A 42 == f (A 42)
    assert $ B false == f (B true)
    assert $ C "Hi" == f (C "Hi")
    assert $ Nothing == preview _B (A 42)
    assert $ Just true == preview _B (B true)
    assert $ Nothing == preview _A (C "Hi")

  log "Example _C" *> do
    let f = over _C (_ <> "!")
    assert $ A 42 == f (A 42)
    assert $ B true == f (B true)
    assert $ C "Hi!" == f (C "Hi")
    assert $ Nothing == preview _C (A 42)
    assert $ Nothing == preview _C (B true)
    assert $ Just "Hi" == preview _C (C "Hi")

  log "All done!"

