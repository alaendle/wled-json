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

prop :: StateComplete -> StatePatch -> Bool
prop c p = c == append c (diff (append c p) c)

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
      it "do not append lists" $ do
        let complete :: StateComplete = State True 255 0 0 0 (Nightlight False 0 0 0 0) 0 0 []
        let patch :: StatePatch = (mempty :: StatePatch) { stateSeg = Just [(mempty :: SegmentPatch) { segmentBri = Just 255 }] }
        append complete patch `shouldBe` complete
      it "append/diff" $
        property prop
      it "what" $ do
        let c :: StateComplete = State {stateOn = False, stateBri = 0, stateTransition = 1, statePs = -1, statePl = 0, stateNl = Nightlight {nightlightOn = True, nightlightDur = 0, nightlightMode = 1, nightlightTbri = 0, nightlightRem = 1}, stateLor = -1, stateMainseg = 0, stateSeg = [Segment {segmentId = 1, segmentStart = 1, segmentStop = 1, segmentLen = 0, segmentGrp = -1, segmentSpc = 1, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 1, segmentCct = 0, segmentSet = -1, segmentCol = [[]], segmentFx = 0, segmentSx = 0, segmentIx = 1, segmentPal = 0, segmentC1 = 1, segmentC2 = -1, segmentC3 = -1, segmentSel = True, segmentRev = True, segmentMi = False, segmentO1 = False, segmentO2 = True, segmentO3 = False, segmentSi = 1, segmentM12 = -1}]}
        let p :: StatePatch = State {stateOn = Just False, stateBri = Just (-1), stateTransition = Nothing, statePs = Just 0, statePl = Just (-1), stateNl = Just (Nightlight {nightlightOn = Nothing, nightlightDur = Just 0, nightlightMode = Nothing, nightlightTbri = Just (-1), nightlightRem = Just 0}), stateLor = Just 1, stateMainseg = Just (-1), stateSeg = Just []}
        print $ append c p
        print $ diff (append c p) c
        c `shouldBe` append c (diff (append c p) c)
