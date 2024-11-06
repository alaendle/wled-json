{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( getLampState,
      setLampState
    ) where

import           Data.Aeson                  (eitherDecodeStrict, encode)
import           Data.ByteString             (ByteString, toStrict)
import           Network.HTTP.Client.Conduit (Request (method), path)
import           Network.HTTP.Simple         (getResponseBody, httpBS, parseRequest, setRequestBodyJSON)
import           Types                       (StateComplete, StatePatch)


-- >>> getLampState "http://192.168.178.34"
-- Right (State {stateOn = True, stateBri = 10, stateTransition = 7, statePs = -1, statePl = -1, stateLor = 0, stateMainseg = 0})
getLampState :: String -> IO (Either String StateComplete)
getLampState wledUrl = do
    req <- parseRequest wledUrl
    res <- httpBS req { path = "json/state" }
    pure (eitherDecodeStrict $ getResponseBody res)

-- >>> setLampState "http://192.168.178.34" $ (mempty :: StatePatch) { stateBri = Just 10 } <> (mempty :: StatePatch) { stateOn = Nothing }
-- ("{\"bri\":10}","{\"success\":true}")
setLampState :: String -> StatePatch -> IO (ByteString, ByteString)
setLampState wledUrl patch =
    let body = encode patch
    in do
      req <- parseRequest wledUrl
      res <- httpBS $ setRequestBodyJSON patch req { method = "POST", path = "json/state" }
      pure (toStrict body, getResponseBody res)

