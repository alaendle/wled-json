{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}

module Types (State (..), Nightlight (..), Segment (..), StateComplete, StatePatch, NightlightComplete, NightlightPatch, SegmentComplete, SegmentPatch) where

import           Barbies.Bare
import           Control.Applicative      (Alternative ((<|>)), empty)
import qualified Data.Aeson               as A
import           Data.Char                (toLower)
import           Data.Functor.Barbie
import           Data.Functor.Identity    (Identity)
import           Data.Functor.Transformer
import           Data.Kind                (Type)
import           Deriving.Aeson

-- | State data type.
type State :: Type -> (Type -> Type)-> (Type -> Type) -> Type
data State t f f' = State
    { stateOn         :: Wear t f Bool
    , stateBri        :: Wear t f Int
    , stateTransition :: Wear t f Int
    , statePs         :: Wear t f Int
    , statePl         :: Wear t f Int
    , stateNl         :: Wear t f (Nightlight t f')
--    , udpn :: Wear t f UdpNetwork
    , stateLor        :: Wear t f Int
    , stateMainseg    :: Wear t f Int
    , stateSeg        :: Wear t f [Segment t f']
    } deriving stock (Generic) --, ConstraintsB, FunctorB, ApplicativeB)

instance Functor f => FunctorB (State Covered f)
instance FunctorB (State Bare f)

instance FunctorT (State Covered)
instance ApplicativeT (State Covered)

deriving stock instance Show (State Bare f f')
deriving stock instance Eq (State Bare f f')
deriving stock instance (Show (f Bool), Show (f Int), Show (f (Nightlight Covered f')), Show (f [Segment Covered f'])) => Show (State Covered f f')
deriving stock instance (Eq (f Bool), Eq (f Int), Eq (f (Nightlight Covered f')), Eq (f [Segment Covered f'])) => Eq (State Covered f f')
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Bare f f') instance A.FromJSON (State Bare f f')
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Bare f f') instance A.ToJSON (State Bare f f')
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Covered f f') instance (FromJSON (f Bool), FromJSON (f Int), FromJSON (f (Nightlight Covered f')), FromJSON (f [Segment Covered f'])) => A.FromJSON (State Covered f f')
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Covered f f') instance (ToJSON (f Bool), ToJSON (f Int), ToJSON (f (Nightlight Covered f')), ToJSON (f [Segment Covered f'])) => A.ToJSON (State Covered f f')

instance (Alternative f) => Semigroup (State Covered f f') where
  (<>) = tzipWith (<|>)

instance (Alternative f) => Monoid (State Covered f f') where
  mempty = tpure empty

-- | Nightlight data type.
type Nightlight :: Type -> (Type -> Type) -> Type
data Nightlight t f = Nightlight
    { nightlightOn   :: Wear t f Bool
    , nightlightDur  :: Wear t f Int
    , nightlightMode ::Wear t f  Int
    , nightlightTbri ::Wear t f  Int
    , nightlightRem  ::Wear t f  Int
    } deriving stock (Generic)

instance ConstraintsB (Nightlight Covered)
instance FunctorB (Nightlight Covered)
instance ApplicativeB (Nightlight Covered)

instance ConstraintsB (Nightlight Bare)
instance FunctorB (Nightlight Bare)

deriving stock instance (AllBF Show f (Nightlight Bare)) => Show (Nightlight Bare f)
deriving stock instance (AllBF Eq f (Nightlight Bare)) => Eq (Nightlight Bare f)
deriving stock instance (AllBF Show f (Nightlight Covered)) => Show (Nightlight Covered f)
deriving stock instance (AllBF Eq f (Nightlight Covered)) => Eq (Nightlight Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "nightlight", ToLower]] (Nightlight Bare f) instance (AllBF A.FromJSON f (Nightlight Bare)) => A.FromJSON (Nightlight Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "nightlight", ToLower]] (Nightlight Bare f) instance (AllBF A.ToJSON f (Nightlight Bare)) => A.ToJSON (Nightlight Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "nightlight", ToLower]] (Nightlight Covered f) instance (AllBF A.FromJSON f (Nightlight Covered)) => A.FromJSON (Nightlight Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "nightlight", ToLower]] (Nightlight Covered f) instance (AllBF A.ToJSON f (Nightlight Covered)) => A.ToJSON (Nightlight Covered f)

instance (Alternative f) => Semigroup (Nightlight Covered f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (Nightlight Covered f) where
  mempty = bpure empty

-- | Segment data type.
type Segment :: Type -> (Type -> Type) -> Type
data Segment t f = Segment
    { segmentId    :: Wear t f Int
    , segmentStart :: Wear t f Int
    , segmentStop  :: Wear t f Int
    , segmentLen   :: Wear t f Int
    , segmentGrp   :: Wear t f Int
    , segmentSpc   :: Wear t f Int
    , segmentOf    :: Wear t f Int
    , segmentOn    :: Wear t f Bool
    , segmentFrz   :: Wear t f Bool
    , segmentBri   :: Wear t f Int
    , segmentCct   :: Wear t f Int
    , segmentSet   :: Wear t f Int
    , segmentCol   :: Wear t f [[Int]]
    , segmentFx    :: Wear t f Int
    , segmentSx    :: Wear t f Int
    , segmentIx    :: Wear t f Int
    , segmentPal   :: Wear t f Int
    , segmentC1    :: Wear t f Int
    , segmentC2    :: Wear t f Int
    , segmentC3    :: Wear t f Int
    , segmentSel   :: Wear t f Bool
    , segmentRev   :: Wear t f Bool
    , segmentMi    :: Wear t f Bool
    , segmentO1    :: Wear t f Bool
    , segmentO2    :: Wear t f Bool
    , segmentO3    :: Wear t f Bool
    , segmentSi    :: Wear t f Int
    , segmentM12   :: Wear t f Int
    } deriving stock (Generic)

instance ConstraintsB (Segment Covered)
instance FunctorB (Segment Covered)
instance ApplicativeB (Segment Covered)

instance ConstraintsB (Segment Bare)
instance FunctorB (Segment Bare)

deriving stock instance (AllBF Show f (Segment Bare)) => Show (Segment Bare f)
deriving stock instance (AllBF Eq f (Segment Bare)) => Eq (Segment Bare f)
deriving stock instance (AllBF Show f (Segment Covered)) => Show (Segment Covered f)
deriving stock instance (AllBF Eq f (Segment Covered)) => Eq (Segment Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "segment", ToLower]] (Segment Bare f) instance (AllBF A.FromJSON f (Segment Bare)) => A.FromJSON (Segment Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "segment", ToLower]] (Segment Bare f) instance (AllBF A.ToJSON f (Segment Bare)) => A.ToJSON (Segment Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "segment", ToLower]] (Segment Covered f) instance (AllBF A.FromJSON f (Segment Covered)) => A.FromJSON (Segment Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "segment", ToLower]] (Segment Covered f) instance (AllBF A.ToJSON f (Segment Covered)) => A.ToJSON (Segment Covered f)

instance (Alternative f) => Semigroup (Segment Covered f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (Segment Covered f) where
  mempty = bpure empty

type ToLower :: Type
data ToLower
instance StringModifier ToLower where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = toLower c : xs

type StateComplete :: Type
type StateComplete = State Bare Identity Identity

type StatePatch :: Type
type StatePatch = State Covered Maybe Maybe

type NightlightComplete :: Type
type NightlightComplete = Nightlight Bare Identity

type NightlightPatch :: Type
type NightlightPatch = Nightlight Covered Maybe

type SegmentComplete :: Type
type SegmentComplete = Segment Bare Identity

type SegmentPatch :: Type
type SegmentPatch = Segment Covered Maybe
