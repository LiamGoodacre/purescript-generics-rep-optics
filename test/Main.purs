module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Optic (ctor, ctorArgs, _0, _1)
import Type.Data.Symbol (SProxy(..))
import Data.Lens (over, view, preview, Lens', Traversal', to, Iso', Prism')
import Data.Maybe (Maybe(..))
import Test.Assert (ASSERT, assert)
import Data.Array (singleton)

-- single constructor single argument
data I = I String
derive instance genericI :: Generic I _
derive instance eqI :: Eq I
_I :: Iso' I String
_I = ctor (SProxy :: SProxy "I")

-- multiple constructors, single argument
data E = A Int | B Boolean
derive instance genericE :: Generic E _
derive instance eqE :: Eq E
_A :: Prism' E Int
_A = ctor (SProxy :: SProxy "A")
_B :: Prism' E Boolean
_B = ctor (SProxy :: SProxy "B")

-- product structure
data P = P Int String
derive instance genericP :: Generic P _
_P = SProxy :: SProxy "P"
_P_0 :: Lens' P Int
_P_0 = ctorArgs _P _0
_P_1 :: Lens' P String
_P_1 = ctorArgs _P _1

-- record structure
data R = R { a :: Int, b :: String }
derive instance genericR :: Generic R _
_R = SProxy :: SProxy "R"
_a = SProxy :: SProxy "a"
_b = SProxy :: SProxy "b"
_R_a :: Lens' R Int
_R_a = ctorArgs _R _a
_R_b :: Lens' R String
_R_b = ctorArgs _R _b

-- parameterised structure
data G t = G t t
derive instance genericG :: Generic (G t) _
_G = SProxy :: SProxy "G"
_G_0 :: forall t. Lens' (G t) t
_G_0 = ctorArgs _G _0
_G_1 :: forall t. Lens' (G t) t
_G_1 = ctorArgs _G _1

-- nested structures
data N = N (G Int) (G String)
derive instance genericN :: Generic N _
_N = SProxy :: SProxy "N"
_N_0 :: Lens' N (G Int)
_N_0 = ctorArgs _N _0
_N_1 :: Lens' N (G String)
_N_1 = ctorArgs _N _1

-- open rows act like regular arguments
data OR r = OR {a :: Int | r}
derive instance genericOR :: Generic (OR r) _
_OR :: forall r. Iso' (OR r) {a :: Int | r}
_OR = ctor (SProxy :: SProxy "OR")

-- multiple constructors, multiple arguments
data ME = MA Int String | MB { a :: Int, b :: String }
derive instance genericME :: Generic ME _
_MA = SProxy :: SProxy "MA"
_MB = SProxy :: SProxy "MB"
_MA_0 :: Traversal' ME Int
_MA_0 = ctorArgs _MA _0
_MA_1 :: Traversal' ME String
_MA_1 = ctorArgs _MA _1
_MB_a :: Traversal' ME Int
_MB_a = ctorArgs _MB _a
_MB_b :: Traversal' ME String
_MB_b = ctorArgs _MB _b

main :: Eff (console :: CONSOLE, assert :: ASSERT) Unit
main = do
  log "I" *> do
    assert $ I "Hi!" == over _I (_ <> "!") (I "Hi")
    assert $ "Hi"    == view _I (I "Hi")

  log "E _A" *> do
    let f = over _A (10 * _)
    assert $ A 420  == f (A 42)
    assert $ B true == f (B true)
    assert $ Just 42 == preview _A (A 42)
    assert $ Nothing == preview _A (B true)

  log "E _B" *> do
    let f = over _B not
    assert $ A 42    == f (A 42)
    assert $ B false == f (B true)
    assert $ Nothing   == preview _B (A 42)
    assert $ Just true == preview _B (B true)

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

  log "OR" *> do
    assert $ 42 == (view _OR (OR {a: 42})).a

  log "ME" *> do
    let ma = MA 42 "Hi"
    let mb = MB { a: 42, b: "Hi" }
    assert $ [42]   == view (_MA_0 <<< to singleton) ma
    assert $ ["Hi"] == view (_MA_1 <<< to singleton) ma
    assert $ [42]   == view (_MB_a <<< to singleton) mb
    assert $ ["Hi"] == view (_MB_b <<< to singleton) mb

  log "All done!"
