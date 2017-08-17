module Data.Generic.Rep.Optic
  ( genericCtor'
  , genericArg'
  , _0
  , _1
  , _2
  , _3
  , _4
  , _5
  , _6
  , _7
  , _8
  , _9
  ) where

import Data.Generic.Rep.Optic.Internal
import Prelude ((<<<))
import Data.Profunctor (class Profunctor)
import Type.Data.Symbol (SProxy(..))
import Data.Generic.Rep (class Generic, Argument)

-- | Profunctor optic into a single argument constructor of a data type
-- | with a generic representation.
-- |
-- | Due to limitations of generics-rep, record arguments do not count as
-- | single arguments.
-- |
-- | If there is only one constructor then the optic is an `Iso'`, otherwise
-- | we get a `Prism'`.
genericCtor' :: forall dt rep ct arg p.
  Profunctor p =>
  Generic dt rep =>
  Ctor ct rep (Argument arg) p =>
  SProxy ct ->
  p arg arg ->
  p dt dt
genericCtor' ct = iRep <<< gCtor' ct <<< iArg

-- | Profunctor optic into a named argument of a named
-- | constructor of a data type with a generic representation.
-- |
-- | If there are multiple constructors in the data type then
-- | we will require `Choice p`.  If there are multiple fields
-- | in the selected constructor, then we require `Strong p`.
-- | Depending on these constraints you will get either a
-- | `Prism'`, a `Lens'`, or a `Traversal'`.
genericArg' :: forall dt rep ct fd arg p.
  Profunctor p =>
  Generic dt rep =>
  CtorArg ct fd rep arg p =>
  SProxy ct ->
  SProxy fd ->
  p arg arg ->
  p dt dt
genericArg' ct fd = iRep <<< gCtorArg' ct fd

_0 :: SProxy "value0"
_0 = SProxy

_1 :: SProxy "value1"
_1 = SProxy

_2 :: SProxy "value2"
_2 = SProxy

_3 :: SProxy "value3"
_3 = SProxy

_4 :: SProxy "value4"
_4 = SProxy

_5 :: SProxy "value5"
_5 = SProxy

_6 :: SProxy "value6"
_6 = SProxy

_7 :: SProxy "value7"
_7 = SProxy

_8 :: SProxy "value8"
_8 = SProxy

_9 :: SProxy "value9"
_9 = SProxy

