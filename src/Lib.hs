{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.ByteString (ByteString, toStrict)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest, setRequestBodyJSON)
import Types
import Data.Aeson (eitherDecodeStrict, encode)
import Network.HTTP.Client.Conduit (Request(method))


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- >>> simpleHttpGet
-- Right (State {stateOn = True, stateBri = 255, stateTransition = 7, statePs = -1, statePl = -1, stateLor = 0, stateMainseg = 1})
simpleHttpGet :: IO (Either String StateComplete)
simpleHttpGet = do
    response <- httpBS =<< parseRequest "http://192.168.178.34/json/state"
    pure (eitherDecodeStrict $ getResponseBody response)

-- >>> simpleHttpSet
-- ("{\"on\":true}","{\"success\":true}")
simpleHttpSet :: IO (ByteString, ByteString)
simpleHttpSet =
    let patch0 :: StatePatch = (mempty :: StatePatch) { stateBri = Nothing }
        patch1 :: StatePatch = (mempty :: StatePatch) { stateOn = Just True }
        patch = patch0 <> patch1
        body = encode patch
    in do
      req <- parseRequest "http://192.168.178.34/json/state"
      res <- httpBS $ setRequestBodyJSON patch $ req { method = "POST" }
      pure (toStrict body, getResponseBody res)

