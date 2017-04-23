module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Optic (ctor, ctorArgs, _0, _1)
import Type.Data.Symbol (SProxy(..))
import Data.Lens (over, view, preview, Lens')
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)

data I = I String
derive instance genericI :: Generic I _
derive instance eqI :: Eq I
_I = SProxy :: SProxy "I"

data E = A Int | B Boolean
derive instance genericE :: Generic E _
derive instance eqE :: Eq E
_A = SProxy :: SProxy "A"
_B = SProxy :: SProxy "B"

data P = P Int String
derive instance genericP :: Generic P _
_P = SProxy :: SProxy "P"
_P_0 :: Lens' P Int
_P_0 = ctorArgs _P _0
_P_1 :: Lens' P String
_P_1 = ctorArgs _P _1

data R = R { a :: Int, b :: String }
derive instance genericR :: Generic R _
_R = SProxy :: SProxy "R"
_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_R_a :: Lens' R Int
_R_a = ctorArgs _R _a
_R_b :: Lens' R String
_R_b = ctorArgs _R _b

data G t = G t t
derive instance genericG :: Generic (G t) _
_G = SProxy :: SProxy "G"
_G_0 :: forall t. Lens' (G t) t
_G_0 = ctorArgs _G _0
_G_1 :: forall t. Lens' (G t) t
_G_1 = ctorArgs _G _1

data N = N (G Int) (G String)
derive instance genericN :: Generic N _
_N = SProxy :: SProxy "N"
_N_0 :: Lens' N (G Int)
_N_0 = ctorArgs _N _0
_N_1 :: Lens' N (G String)
_N_1 = ctorArgs _N _1

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "I _I" *> do
    assert $ I "Hi!" == over (ctor _I) (_ <> "!") (I "Hi")
    assert $ "Hi" == view (ctor _I) (I "Hi")

  log "E _A" *> do
    let f = over (ctor _A) (10 * _)
    assert $ A 420  == f (A 42)
    assert $ B true == f (B true)
    assert $ Just 42 == preview (ctor _A) (A 42)
    assert $ Nothing == preview (ctor _A) (B true)

  log "E _B" *> do
    let f = over (ctor _B) not
    assert $ A 42    == f (A 42)
    assert $ B false == f (B true)
    assert $ Nothing   == preview (ctor _B) (A 42)
    assert $ Just true == preview (ctor _B) (B true)

  log "P" *> do
    assert $ 42   == view _P_0 (P 42 "Hi")
    assert $ "Hi" == view _P_1 (P 42 "Hi")

  log "R" *> do
    assert $ 42   == view _R_a (R {a: 42, b: "Hi"})
    assert $ "Hi" == view _R_b (R {a: 42, b: "Hi"})

  log "G" *> do
    assert $ "A" == view _G_0 (G "A" "B")
    assert $ "B" == view _G_1 (G "A" "B")

  log "N" *> do
    let n = N (G 1 2) (G "A" "B")
    assert $ 1   == view (_N_0 <<< _G_0) n
    assert $ 2   == view (_N_0 <<< _G_1) n
    assert $ "A" == view (_N_1 <<< _G_0) n
    assert $ "B" == view (_N_1 <<< _G_1) n

  log "All done!"
