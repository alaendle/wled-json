{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Types (State (..), StateComplete, StatePatch) where

import Control.Applicative (Alternative ((<|>)))
import qualified Data.Aeson as A
import Data.Char (toLower)
import Data.Functor.Barbie
import Deriving.Aeson
import Barbies.Bare
import Data.Functor.Identity (Identity)

data State t f = State
    { stateOn :: Wear t f Bool
    , stateBri :: Wear t f Int
    , stateTransition :: Wear t f Int
    , statePs :: Wear t f Int
    , statePl :: Wear t f Int
--    , nl :: Wear t f Nightlight
--    , udpn :: Wear t f UdpNetwork
    , stateLor :: Wear t f Int
    , stateMainseg :: Wear t f Int
--    , seg :: Wear t f [Segment]
    } deriving (Generic) --, ConstraintsB, FunctorB, ApplicativeB)

instance ConstraintsB (State Covered)
instance FunctorB (State Covered)
instance ApplicativeB (State Covered)

instance ConstraintsB (State Bare)
instance FunctorB (State Bare)

deriving instance (AllBF Show f (State Bare)) => Show (State Bare f)
deriving instance (AllBF Eq f (State Bare)) => Eq (State Bare f)
deriving instance (AllBF Show f (State Covered)) => Show (State Covered f)
deriving instance (AllBF Eq f (State Covered)) => Eq (State Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Bare f) instance (AllBF A.FromJSON f (State Bare)) => A.FromJSON (State Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Bare f) instance (AllBF A.ToJSON f (State Bare)) => A.ToJSON (State Bare f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Covered f) instance (AllBF A.FromJSON f (State Covered)) => A.FromJSON (State Covered f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State Covered f) instance (AllBF A.ToJSON f (State Covered)) => A.ToJSON (State Covered f)

instance (Alternative f) => Semigroup (State Covered f) where
  (<>) = bzipWith (<|>)

data ToLower
instance StringModifier ToLower where
  getStringModifier "" = ""
  getStringModifier (c : xs) = toLower c : xs

type StateComplete = State Bare Identity

type StatePatch = State Covered Maybe
