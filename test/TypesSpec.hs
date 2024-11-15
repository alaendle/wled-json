{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module TypesSpec (spec) where

import           Data.Aeson
import           Data.Aeson.Key
import           Data.Aeson.KeyMap
import           Test.Hspec
import           Test.QuickCheck
import           Types

instance Arbitrary StatePatch where
  arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SegmentPatch where
  arbitrary = Segment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NightlightPatch where
  arbitrary = Nightlight <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_associativity :: StatePatch -> StatePatch -> StatePatch -> Bool
prop_associativity a b c = (a <> b) <> c == a <> (b <> c)

prop_identity :: StatePatch -> Bool
prop_identity a = mempty <> a == a && a <> mempty == a

spec :: Spec
spec = do
  describe "Types" $ do
    describe "StatePatch" $ do
      it "Omit nothing fields" $ do
        let patch :: StatePatch = (mempty :: StatePatch) { stateBri = Just 1 }
        let json :: Either String Value = eitherDecode $ encode patch
        json `shouldBe` Right (Object (fromList [(fromString "bri",Number 1)]))
      it "should satisfy the associativity law" $
        property prop_associativity
      it "should have an identity" $
        property prop_identity
