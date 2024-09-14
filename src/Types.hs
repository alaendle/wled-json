{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module Types (State(..)) where
import Data.Functor.Barbie
import GHC.Generics (Generic)
import qualified Data.Aeson as A
import Deriving.Aeson
import Data.Char (toLower)

data State f = State
    { stateOn :: f Bool
    , stateBri :: f Int
    , stateTransition :: f Int
    , statePs :: f Int
    , statePl :: f Int
--    , nl :: f Nightlight
--    , udpn :: f UdpNetwork
    , stateLor :: f Int
    , stateMainseg :: f Int
--    , seg :: f [Segment]
    } deriving (Generic, ConstraintsB, FunctorB, ApplicativeB)

deriving instance (AllBF Show f State) => Show (State f)
deriving instance (AllBF Eq f State) => Eq (State f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State f) instance (AllBF A.FromJSON f State) => A.FromJSON (State f)
deriving via CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "state", ToLower]] (State f) instance (AllBF A.ToJSON f State) => A.ToJSON (State f)

data ToLower
instance StringModifier ToLower where
  getStringModifier "" = ""
  getStringModifier (c : xs) = toLower c : xs