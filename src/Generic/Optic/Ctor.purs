module Generic.Optic.Ctor
  ( noCtor
  , genericAbsurd
  , _Ctor'
  ) where

import Prelude
  ( (<<<), Void, absurd )
import Data.Generic.Rep
  ( class Generic, NoConstructors )
import Data.Profunctor
  ( class Profunctor )
import Data.Symbol
  ( SProxy )
import Generic.Optic.Rep
  ( rep', noConstructors )
import Data.Lens
  ( view )
import Generic.Optic.Ctor.Internal
  ( class GenericCtor, repCtor' )

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

