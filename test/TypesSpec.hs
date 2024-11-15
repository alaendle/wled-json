{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Monoid law, left identity" #-}
{-# HLINT ignore "Monoid law, right identity" #-}

module TypesSpec (spec) where

import           Data.Aeson
import           Data.Aeson.Key
import           Data.Aeson.KeyMap
import           Test.Hspec
import           Test.QuickCheck
import           Types

instance Arbitrary StatePatch where
  arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary StateComplete where
  arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SegmentPatch where
  arbitrary = Segment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary SegmentComplete where
  arbitrary = Segment <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
                      <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NightlightPatch where
  arbitrary = Nightlight <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary NightlightComplete where
  arbitrary = Nightlight <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

prop_associativity :: StatePatch -> StatePatch -> StatePatch -> Bool
prop_associativity a b c = (a <> b) <> c == a <> (b <> c)

prop_identity :: StatePatch -> Bool
prop_identity a = mempty <> a == a && a <> mempty == a

--prop :: StateComplete -> StatePatch -> Bool
--prop d dx = let s = append d dx in append d (diff d s) == s

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
--      it "append/diff" $
--        property prop
