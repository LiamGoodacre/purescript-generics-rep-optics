module Generic.Optic.Ctor.Internal
  ( repCtor'
  , class GenericCtor
  , _GenericCtor
  , class GenericCtorArg
  , _GenericCtorArg
  ) where

import Prelude ((<<<), Unit, unit)
import Data.Generic.Rep
  ( Constructor, Argument, Sum, NoArguments, Product
  )
import Prim.TypeError (class Fail, Text, Beside)
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Choice (class Choice)
import Data.Symbol (SProxy)
import Generic.Optic.Rep
  ( constructor, sumInl, sumInr, argument, noArguments )

infixl 4 type Beside as <>

repCtor' ::
  forall ctor s a p .
  GenericCtor p ctor s a =>
  SProxy ctor ->
  p a a -> p s s
repCtor' ctor = _GenericCtor ctor

class GenericCtor p ctor rep a | ctor rep -> a where
  _GenericCtor :: SProxy ctor -> p a a -> p rep rep

class GenericCtorArg ctor p arg a | arg -> a where
  _GenericCtorArg :: SProxy ctor -> p a a -> p arg arg

instance genericCtorSumFound ::
  ( Choice p
  , GenericCtorArg ctor p arg a
  ) =>
  GenericCtor p ctor (Sum (Constructor ctor arg) r) a where
    _GenericCtor ctor = sumInl <<< constructor <<< _GenericCtorArg ctor
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
  , GenericCtorArg ctor p arg a
  ) =>
  GenericCtor p ctor (Constructor ctor arg) a where
    _GenericCtor ctor = constructor <<< _GenericCtorArg ctor
else
instance genericCtorSumFail ::
  Fail (
    Text "No constructors found called `" <>
    Text ctor <>
    Text "`" ) =>
  GenericCtor p ctor (Constructor other b) a where
    _GenericCtor _ = loop unit where
      loop :: forall w . Unit -> w
      loop u = loop u

instance genericCtorArgMatch ::
  Profunctor p =>
  GenericCtorArg ctor p (Argument a) a where
    _GenericCtorArg _ = argument
else
instance genericCtorArgNone ::
  Profunctor p =>
  GenericCtorArg ctor p NoArguments Unit where
    _GenericCtorArg _ = noArguments
else
instance genericCtorArgFail ::
  Fail (
    Text "Multiple arguments found for constructor `" <>
    Text ctor <>
    Text "`" ) =>
  GenericCtorArg ctor p (Product l r) a where
    _GenericCtorArg _ = loop unit where
      loop :: forall w . Unit -> w
      loop u = loop u


