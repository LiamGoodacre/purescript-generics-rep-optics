module Data.Generic.Rep.Optic.Internal
  ( iRep
  , class Ctor
  , gCtor'
  , class CtorArg
  , gCtorArg'
  , iArg
  , class CtorSum
  , gCtorSum
  , class Args
  , gArgs'
  , Z
  , S
  , class NatKey
  , class ArgsProd
  , gArgsProd
  , class ArgsRec
  , gArgsRec
  , class ArgsRecCheck
  , gArgsRecCheck
  ) where

import Data.Generic.Rep
import Prelude ((<<<))
import Data.Lens (Iso, iso, Prism, prism, Lens, lens)
import Type.Data.Symbol (SProxy, class CompareSymbol)
import Type.Data.Ordering (OProxy(..), EQ, LT, GT, kind Ordering)
import Type.Proxy (Proxy(..))
import Data.Profunctor (class Profunctor)
import Data.Profunctor.Strong (class Strong)
import Data.Profunctor.Choice (class Choice)
import Data.Either (Either(..))

-- | Isomorphism between data and it's generic representation
iRep :: forall s a. Generic s a => Iso s s a a
iRep = iso from to

iCtor :: forall c a b. Iso (Constructor c a) (Constructor c b) a b
iCtor = iso (\(Constructor a) -> a) Constructor

iArg :: forall a b. Iso (Argument a) (Argument b) a b
iArg = iso (\(Argument a) -> a) Argument

iField :: forall fd a b. Iso (Field fd a) (Field fd b) a b
iField = iso (\(Field a) -> a) Field

-- | Prism into the left of a Sum
gInl :: forall a b r. Prism (Sum a r) (Sum b r) a b
gInl = prism Inl alpha where
  alpha (Inl l) = Right l
  alpha (Inr r) = Left (Inr r)

-- | Prism into the right of a Sum
gInr :: forall l a b. Prism (Sum l a) (Sum l b) a b
gInr = prism Inr alpha where
  alpha (Inr r) = Right r
  alpha (Inl l) = Left (Inl l)

gProl :: forall a b r. Lens (Product a r) (Product b r) a b
gProl = lens (\(Product l r) -> l) (\(Product _ r) l -> Product l r)

gPror :: forall l a b. Lens (Product l a) (Product l b) a b
gPror = lens (\(Product l r) -> r) (\(Product l _) r -> Product l r)

-- | Optic into a ctor of a Sum
class Ctor (ct :: Symbol)
           (rep :: Type)
           (out :: Type)
           (p :: Type -> Type -> Type)
           | ct rep -> out p where
  gCtor' :: SProxy ct ->
            p out out ->
            p rep rep

-- | Rep only has a ctor
instance ctorCtor
  :: Profunctor p
  => Ctor ct (Constructor ct arg) arg p where
  gCtor' _ = iCtor

-- | Dispatch on equal symbols
class CtorSum (ord :: Ordering)
              (ct :: Symbol)
              (rep :: Type)
              (out :: Type)
              (p :: Type -> Type -> Type)
              | ord -> ct rep out p where
  gCtorSum :: OProxy ord ->
              SProxy ct ->
              p out out ->
              p rep rep
instance ctorSumEQ
  :: Choice p
  => CtorSum EQ ct (Sum (Constructor ct arg) rest) arg p where
  gCtorSum _ _ = gInl <<< iCtor
instance ctorSumLT
  :: ( Choice p
     , Ctor ct rest out p )
  => CtorSum LT ct (Sum lhs rest) out p where
  gCtorSum _ ct = gInr <<< gCtor' ct
instance ctorSumGT
  :: ( Choice p
     , Ctor ct rest out p )
  => CtorSum GT ct (Sum lhs rest) out p where
  gCtorSum _ ct = gInr <<< gCtor' ct

-- | Rep has multiple ctors
instance ctorSum
  :: ( Choice p
     , CompareSymbol ct cur ord
     , CtorSum ord ct (Sum (Constructor cur arg) rest) out p
     )
  => Ctor ct (Sum (Constructor cur arg) rest) out p where
  gCtor' ct = gCtorSum (OProxy :: OProxy ord) ct

class Args (fd :: Symbol)
           (rep :: Type)
           (out :: Type)
           (p :: Type -> Type -> Type)
           | fd rep -> out p where
  gArgs' :: SProxy fd ->
           p out out ->
           p rep rep
instance argsProd
  :: ( NatKey n fd
     , ArgsProd n (Product l r) out p )
  => Args fd (Product l r) out p where
  gArgs' fd = gArgsProd (Proxy :: Proxy n)
instance argsRec
  :: ( Profunctor p
     , ArgsRec fd r o p )
  => Args fd (Rec r) o p where
  gArgs' fd = iso (\(Rec r) -> r) Rec <<< gArgsRec fd

foreign import data Z :: Type
foreign import data S :: Type -> Type

class NatKey (n :: Type) (s :: Symbol) | n -> s, s -> n
instance natKey0 :: NatKey Z "value0"
instance natKey1 :: NatKey (S Z) "value1"
instance natKey2 :: NatKey (S (S Z)) "value2"
instance natKey3 :: NatKey (S (S (S Z))) "value3"
instance natKey4 :: NatKey (S (S (S (S Z)))) "value4"
instance natKey5 :: NatKey (S (S (S (S (S Z))))) "value5"
instance natKey6 :: NatKey (S (S (S (S (S (S Z)))))) "value6"
instance natKey7 :: NatKey (S (S (S (S (S (S (S Z))))))) "value7"
instance natKey8 :: NatKey (S (S (S (S (S (S (S (S Z)))))))) "value8"
instance natKey9 :: NatKey (S (S (S (S (S (S (S (S (S Z))))))))) "value9"

class ArgsProd (count :: Type)
               (rep :: Type)
               (out :: Type)
               (p :: Type -> Type -> Type)
               | count rep -> out p where
  gArgsProd :: Proxy count ->
               p out out ->
               p rep rep
instance argsProdBaseA
  :: Profunctor p
  => ArgsProd Z (Argument arg) arg p where
  gArgsProd z = iArg
instance argsProdBaseP
  :: Strong p
  => ArgsProd Z (Product (Argument arg) r) arg p where
  gArgsProd z = gProl <<< iArg
instance argsProdStep
  :: ( Strong p
     , ArgsProd n r out p )
  => ArgsProd (S n) (Product l r) out p where
  gArgsProd _ = gPror <<< gArgsProd (Proxy :: Proxy n)

class ArgsRec (fd :: Symbol)
              (rep :: Type)
              (out :: Type)
              (p :: Type -> Type -> Type)
              | rep -> out p where
  gArgsRec :: SProxy fd ->
              p out out ->
              p rep rep
instance argsRecBase
  :: Profunctor p
  => ArgsRec fd (Field fd t) t p where
  gArgsRec _ = iField
instance argsRecStep
  :: ( Strong p
     , CompareSymbol fd lfd ord
     , ArgsRecCheck ord fd (Product (Field lfd lty) r) lfd lty r out p )
  => ArgsRec fd (Product (Field lfd lty) r) out p where
  gArgsRec = gArgsRecCheck (OProxy :: OProxy ord)

class ArgsRecCheck (ord :: Ordering)
                   (fd :: Symbol)
                   (rep :: Type)
                   (lfd :: Symbol)
                   (lty :: Type)
                   (right :: Type)
                   (out :: Type)
                   (p :: Type -> Type -> Type)
                   | ord fd -> rep lfd lty right out p where
  gArgsRecCheck :: OProxy ord ->
                   SProxy fd ->
                   p out out ->
                   p rep rep
instance argsRecCheckEQ
  :: Strong p
  => ArgsRecCheck EQ fd (Product (Field lfd lty) right) lfd lty right lty p where
  gArgsRecCheck _ fd = gProl <<< iField
instance argsRecCheckLT
  :: ( Strong p
     , ArgsRec fd right out p )
  => ArgsRecCheck LT fd (Product left right) lfd lty right out p where
  gArgsRecCheck _ fd = gPror <<< gArgsRec fd
instance argsRecCheckGT
  :: ( Strong p
     , ArgsRec fd right out p )
  => ArgsRecCheck GT fd (Product left right) lfd lty right out p where
  gArgsRecCheck _ fd = gPror <<< gArgsRec fd

class CtorArg (ct :: Symbol)
              (fd :: Symbol)
              (rep :: Type)
              (arg :: Type)
              (p :: Type -> Type -> Type)
              | -> ct fd rep arg p where
  gCtorArg' :: SProxy ct -> SProxy fd -> p arg arg -> p rep rep

instance ctorArgs
  :: ( Ctor ct rep args p
     , Args fd args arg p )
  => CtorArg ct fd rep arg p where
  gCtorArg' ct fd = gCtor' ct <<< gArgs' fd
