module Generic.Optic.Rep
  ( rep
  , rep'
  , noConstructors
  , constructor
  , sum
  , sumInl
  , sumInr
  , noArguments
  , argument
  , product
  , productFirst
  , productSecond
  ) where

import Data.Generic.Rep
import Data.Lens (Iso, Iso', Lens, Prism, iso, lens, prism)
import Data.Either (Either(..))
import Data.Tuple (Tuple(..))
import Prelude (const, unit, Unit, Void, absurd)

-- | Iso between types and their generic rep.
rep ::
  forall s t a b .
  Generic s a =>
  Generic t b =>
  Iso s t a b
rep = iso from to

-- | Iso between a type and its rep.
rep' ::
  forall s a .
  Generic s a =>
  Iso' s a
rep' = rep

-- | NoConstructors is equivalent to Void
noConstructors ::
  Iso' NoConstructors Void
noConstructors = iso absurd' absurd where
  absurd' :: forall w . NoConstructors -> w
  absurd' nc = absurd' nc

-- | Iso between a Constructor and its argument rep
constructor ::
  forall n m a b .
  Iso (Constructor n a) (Constructor m b) a b
constructor = iso (\(Constructor a) -> a) Constructor

-- | Iso between Sum and Either
sum ::
  forall a b c d .
  Iso (Sum a b) (Sum c d) (Either a b) (Either c d)
sum = iso
  (case _ of
    Inl a -> Left a
    Inr b -> Right b)
  (case _ of
    Left c -> Inl c
    Right d -> Inr d)

-- | Prism into the Inl of Sum
sumInl ::
  forall a b r .
  Prism (Sum a r) (Sum b r) a b
sumInl = prism Inl alpha where
  alpha (Inl l) = Right l
  alpha (Inr r) = Left (Inr r)

-- | Prism into the Inr of Sum
sumInr ::
  forall l a b .
  Prism (Sum l a) (Sum l b) a b
sumInr = prism Inr alpha where
  alpha (Inr r) = Right r
  alpha (Inl l) = Left (Inl l)

-- | Iso between NoArguments and Unit
noArguments ::
  Iso' NoArguments Unit
noArguments = iso (const unit) (const NoArguments)

-- | Iso between Argument and its wrapped type
argument ::
  forall a b .
  Iso (Argument a) (Argument b) a b
argument = iso (\(Argument a) -> a) Argument

-- | Iso between Product and Tuple
product ::
  forall a b c d .
  Iso (Product a b) (Product c d) (Tuple a b) (Tuple c d)
product = iso
  (\(Product a b) -> Tuple a b)
  (\(Tuple c d) -> Product c d)

-- | Lens into the first of a Product
productFirst ::
  forall a b r .
  Lens (Product a r) (Product b r) a b
productFirst = lens (\(Product a _) -> a) (\(Product _ r) b -> Product b r)

-- | Lens into the second of a Product
productSecond ::
  forall l a b .
  Lens (Product l a) (Product l b) a b
productSecond = lens (\(Product _ a) -> a) (\(Product l _) b -> Product l b)

