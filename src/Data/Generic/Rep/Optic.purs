module Data.Generic.Rep.Optic
  ( rep
  , ctor
  , class Ctor
  , gCtor
  , class CtorSum
  , gCtorSum
  ) where

import Prelude ((<<<))
import Data.Generic.Rep (class Generic, from, to, Sum(..), Constructor(..), Argument(..))
import Data.Lens (Iso', iso, Prism', prism')
import Type.Data.Symbol (SProxy, class CompareSymbol)
import Type.Data.Ordering (OProxy(..), EQ, LT, GT, kind Ordering)
import Data.Maybe (Maybe(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)

-- | Isomorphism between data and it's generic representation
rep :: forall s a. Generic s a => Iso' s a
rep = iso from to

-- | Iso over the Arg of a Ctor
gArg :: forall c a. Iso' (Constructor c (Argument a)) a
gArg = iso alpha (Constructor <<< Argument) where
  alpha (Constructor (Argument a)) = a

-- | Prism into the left of a Sum
gInl :: forall l r. Prism' (Sum l r) l
gInl = prism' Inl alpha where
  alpha (Inl l) = Just l
  alpha _ = Nothing

-- | Prism into the right of a Sum
gInr :: forall l r. Prism' (Sum l r) r
gInr = prism' Inr alpha where
  alpha (Inr r) = Just r
  alpha _ = Nothing

-- | Prism into a ctor of a Sum
class Ctor (ct :: Symbol)
           (gen :: Type)
           (out :: Type)
           (p :: Type -> Type -> Type)
           | ct gen -> out p where
  gCtor :: SProxy ct ->
           p out out ->
           p gen gen

-- | Rep only has a ctor
instance ctorCtor
  :: Profunctor p
  => Ctor ct (Constructor ct (Argument arg)) arg p where
  gCtor _ = gArg

-- | Dispatch on equal symbols
class CtorSum (ord :: Ordering)
              (ct :: Symbol)
              (gen :: Type)
              (out :: Type)
              (p :: Type -> Type -> Type)
              | ord -> ct gen out p where
  gCtorSum :: OProxy ord ->
              SProxy ct ->
              p out out ->
              p gen gen
instance ctorSumEQ
  :: Choice p
  => CtorSum EQ ct (Sum (Constructor ct (Argument arg)) rest) arg p where
  gCtorSum _ _ = gInl <<< gArg
instance ctorSumLT
  :: ( Choice p
     , Ctor ct rest out p )
  => CtorSum LT ct (Sum lhs rest) out p where
  gCtorSum _ ct = gInr <<< gCtor ct
instance ctorSumGT
  :: ( Choice p
     , Ctor ct rest out p )
  => CtorSum GT ct (Sum lhs rest) out p where
  gCtorSum _ ct = gInr <<< gCtor ct

-- | Rep has multiple ctors
instance ctorSum
  :: ( Choice p
     , CompareSymbol ct cur ord
     , CtorSum ord ct (Sum (Constructor cur (Argument arg)) rest) out p
     )
  => Ctor ct (Sum (Constructor cur (Argument arg)) rest) out p where
  gCtor ct = gCtorSum (OProxy :: OProxy ord) ct

-- | Optic into a constructor of a data type with a generics-rep instance.
-- | If there is only one constructor then the optic is an Iso, otherwise
-- | we get a Prism.
-- | Example:
-- | ```purescript
-- | -- given the following data types
-- | data I = I String
-- | data E = L Int | R Boolean
-- | derive instance genericI :: Generic I _
-- | derive instance genericE :: Generic E _
-- | _I = SProxy :: SProxy "I"
-- | _L = SProxy :: SProxy "L"
-- | _R = SProxy :: SProxy "R"
-- |
-- | -- the following optics exist
-- | ctor _I :: Iso' I String
-- | ctor _L :: Prism' E Int
-- | ctor _R :: Prism' E Boolean
-- | ```
ctor :: forall d g c a p.
  Profunctor p =>
  Generic d g =>
  Ctor c g a p =>
  SProxy c ->
  p a a ->
  p d d
ctor c = rep <<< gCtor c

