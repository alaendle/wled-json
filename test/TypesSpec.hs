{-# LANGUAGE ScopedTypeVariables #-}

module TypesSpec (spec) where

import           Data.Aeson
import           Data.Aeson.Key
import           Data.Aeson.KeyMap
import           Test.Hspec
import           Types

spec :: Spec
spec = do
  describe "Types" $ do
    describe "StatePatch" $ do
      it "Omit nothing fields" $ do
        let patch :: StatePatch = (mempty :: StatePatch) { stateBri = Just 1 }
        let json :: Either String Value = eitherDecode $ encode patch
        json `shouldBe` Right (Object (fromList [(fromString "bri",Number 1)]))
