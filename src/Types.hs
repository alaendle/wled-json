{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DeriveGeneric            #-}
{-# LANGUAGE DerivingVia              #-}
{-# LANGUAGE FlexibleInstances        #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE UndecidableInstances     #-}

module Types (State (..), StateComplete, StatePatch) where

import           Barbies.Bare
import           Control.Applicative   (Alternative ((<|>)), empty)
import qualified Data.Aeson            as A
import           Data.Char             (toLower)
import           Data.Functor.Barbie
import           Data.Functor.Identity (Identity)
import           Data.Kind             (Type)
import           Deriving.Aeson

type State :: Type -> (Type -> Type) -> Type
data State t f = State
    { stateOn         :: Wear t f Bool
    , stateBri        :: Wear t f Int
    , stateTransition :: Wear t f Int
    , statePs         :: Wear t f Int
    , statePl         :: Wear t f Int
--    , nl :: Wear t f Nightlight
--    , udpn :: Wear t f UdpNetwork
    , stateLor        :: Wear t f Int
    , stateMainseg    :: Wear t f Int
--    , seg :: Wear t f [Segment]
    } deriving stock (Generic) --, ConstraintsB, FunctorB, ApplicativeB)

instance ConstraintsB (State Covered)
instance FunctorB (State Covered)
instance ApplicativeB (State Covered)

instance ConstraintsB (State Bare)
instance FunctorB (State Bare)

deriving stock instance (AllBF Show f (State Bare)) => Show (State Bare f)
deriving stock instance (AllBF Eq f (State Bare)) => Eq (State Bare f)
deriving stock instance (AllBF Show f (State Covered)) => Show (State Covered f)
deriving stock instance (AllBF Eq f (State Covered)) => Eq (State Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Bare f) instance (AllBF A.FromJSON f (State Bare)) => A.FromJSON (State Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Bare f) instance (AllBF A.ToJSON f (State Bare)) => A.ToJSON (State Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Covered f) instance (AllBF A.FromJSON f (State Covered)) => A.FromJSON (State Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Covered f) instance (AllBF A.ToJSON f (State Covered)) => A.ToJSON (State Covered f)

instance (Alternative f) => Semigroup (State Covered f) where
  (<>) = bzipWith (<|>)

instance (Alternative f) => Monoid (State Covered f) where
  mempty = bpure empty

type ToLower :: Type
data ToLower
instance StringModifier ToLower where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = toLower c : xs

type StateComplete :: Type
type StateComplete = State Bare Identity

type StatePatch :: Type
type StatePatch = State Covered Maybe
