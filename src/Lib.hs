{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc
    ) where

import Data.ByteString (ByteString, toStrict)
import Network.HTTP.Simple (getResponseBody, httpBS, parseRequest, setRequestBodyJSON)
import Types
import Data.Functor.Identity (Identity)
import Data.Aeson (eitherDecodeStrict, encode)
import Barbies (bmempty, bpure)
import Network.HTTP.Client.Conduit (Request(method), requestBody)


someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- >>> simpleHttpGet
-- Right (State {stateOn = Identity True, stateBri = Identity 255, stateTransition = Identity 7, statePs = Identity (-1), statePl = Identity (-1), stateLor = Identity 0, stateMainseg = Identity 1})
simpleHttpGet :: IO (Either String (State Identity))
simpleHttpGet = do
    response <- httpBS =<< parseRequest "http://192.168.178.34/json/state"
    pure (eitherDecodeStrict $ getResponseBody response)

-- >>> simpleHttpSet
-- ("{\"on\":true}","{\"success\":true}")
simpleHttpSet :: IO (ByteString, ByteString)
simpleHttpSet =
    let patch0 :: State Maybe = (bpure Nothing) { stateBri = Nothing }
        patch1 :: State Maybe = (bpure Nothing) { stateOn = Just True }
        patch = patch0 <> patch1
        body = encode patch
    in do
      req <- parseRequest "http://192.168.178.34/json/state"
      res <- httpBS $ setRequestBodyJSON patch $ req { method = "POST" }
      pure (toStrict body, getResponseBody res)

