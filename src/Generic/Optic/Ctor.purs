module Generic.Optic.Ctor
  ( noCtor
  , genericAbsurd
  , _Ctor'
  , class GenericCtor
  , _GenericCtor
  , class GenericCtorArg
  , _GenericCtorArg
  ) where

import Prelude ((<<<), Unit, unit, Void, absurd)
import Data.Generic.Rep
  ( class Generic, Constructor, Argument, Sum, NoArguments, Product, NoConstructors
  )
import Prim.TypeError (class Fail, Text)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Symbol (SProxy)
import Generic.Optic.Rep
  ( rep', constructor, sumInl, sumInr, argument, noArguments, noConstructors )
import Data.Lens (view)

-- | Iso from an empty type to Void.
noCtor ::
  forall s p .
  Profunctor p =>
  Generic s NoConstructors =>
  p Void Void -> p s s
noCtor = rep' <<< noConstructors

-- | Absurd for any Generic data-type with no constructors.
genericAbsurd :: forall t a . Generic t NoConstructors => t -> a
genericAbsurd t = absurd (view noCtor t)

-- | Prism into a specific constructor of a data-type.
-- | Requires the constructor name in a symbol.
-- |
-- | Example:
-- |
-- | ``` purescript
-- | data T = A Int | B
-- |
-- | _A :: Prism' T Int
-- | _A = _Ctor' (SProxy :: SProxy "A")
-- |
-- | _B :: Prism' T Unit
-- | _B = _Ctor' (SProxy :: SProxy "B")
-- | ```
_Ctor' ::
  forall ctor s a p rep .
  Profunctor p =>
  Generic s rep =>
  GenericCtor p ctor rep a =>
  SProxy ctor ->
  p a a -> p s s
_Ctor' ctor = rep' <<< repCtor' ctor

repCtor' ::
  forall ctor s a p .
  GenericCtor p ctor s a =>
  SProxy ctor ->
  p a a -> p s s
repCtor' ctor = _GenericCtor ctor

class GenericCtor p ctor rep a | ctor rep -> a where
  _GenericCtor :: SProxy ctor -> p a a -> p rep rep

class GenericCtorArg p arg a | arg -> a where
  _GenericCtorArg :: p a a -> p arg arg

instance genericCtorSumFound ::
  ( Choice p
  , GenericCtorArg p arg a
  ) =>
  GenericCtor p ctor (Sum (Constructor ctor arg) r) a where
    _GenericCtor _ = sumInl <<< constructor <<< _GenericCtorArg
else
instance genericCtorSumNext ::
  ( Choice p
  , GenericCtor p ctor r a
  ) =>
  GenericCtor p ctor (Sum l r) a where
    _GenericCtor ctor = sumInr <<< _GenericCtor ctor
else
instance genericCtorSumLast ::
  ( Profunctor p
  , GenericCtorArg p arg a
  ) =>
  GenericCtor p ctor (Constructor ctor arg) a where
    _GenericCtor _ = constructor <<< _GenericCtorArg
else
instance genericCtorSumFail ::
  Fail (Text "TODO") =>
  GenericCtor p ctor (Constructor other b) a where
    _GenericCtor _ = loop unit where
      loop :: forall w . Unit -> w
      loop u = loop u

instance genericCtorArgMatch ::
  Profunctor p =>
  GenericCtorArg p (Argument a) a where
    _GenericCtorArg = argument
else
instance genericCtorArgNone ::
  Profunctor p =>
  GenericCtorArg p NoArguments Unit where
    _GenericCtorArg = noArguments
else
instance genericCtorArgFail ::
  Fail (Text "TODO") =>
  GenericCtorArg p (Product l r) a where
    _GenericCtorArg = loop unit where
      loop :: forall w . Unit -> w
      loop u = loop u

