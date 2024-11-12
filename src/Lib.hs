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
-- Right (State {stateOn = True, stateBri = 10, stateTransition = 7, statePs = -1, statePl = -1, stateNl = Nightlight {nightlightOn = False, nightlightDur = 60, nightlightMode = 1, nightlightTbri = 0, nightlightRem = -1}, stateLor = 0, stateMainseg = 0, stateSeg = [Segment {segmentId = 0, segmentStart = 0, segmentStop = 5, segmentLen = 5, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = False, segmentFrz = True, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,255,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 1, segmentStart = 5, segmentStop = 16, segmentLen = 11, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,0,0],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 2, segmentStart = 16, segmentStop = 23, segmentLen = 7, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,255,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 3, segmentStart = 23, segmentStop = 34, segmentLen = 11, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[0,0,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 4, segmentStart = 34, segmentStop = 41, segmentLen = 7, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,255,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 5, segmentStart = 41, segmentStop = 52, segmentLen = 11, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[0,0,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 6, segmentStart = 52, segmentStop = 57, segmentLen = 5, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,255,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 7, segmentStart = 57, segmentStop = 68, segmentLen = 11, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,0,0],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0},Segment {segmentId = 8, segmentStart = 68, segmentStop = 101, segmentLen = 33, segmentGrp = 1, segmentSpc = 0, segmentOf = 0, segmentOn = True, segmentFrz = False, segmentBri = 255, segmentCct = 127, segmentSet = 0, segmentCol = [[255,255,255],[0,0,0],[0,0,0]], segmentFx = 0, segmentSx = 128, segmentIx = 128, segmentPal = 0, segmentC1 = 128, segmentC2 = 128, segmentC3 = 16, segmentSel = True, segmentRev = False, segmentMi = False, segmentO1 = False, segmentO2 = False, segmentO3 = False, segmentSi = 0, segmentM12 = 0}]})
getLampState :: String -> IO (Either String StateComplete)
getLampState wledUrl = do
    req <- parseRequest wledUrl
    res <- httpBS req { path = "json/state" }
    pure (eitherDecodeStrict $ getResponseBody res)

-- >>> setLampState "http://192.168.178.34" $ (mempty :: StatePatch) { stateBri = Just 255 } <> (mempty :: StatePatch) { stateOn = Nothing }
-- ("{\"bri\":255}","{\"success\":true}")
-- >>> setLampState "http://192.168.178.34" france
-- ("{\"seg\":[{\"start\":0,\"stop\":5,\"col\":[[255,255,255]]},{\"start\":5,\"stop\":16,\"col\":[[255,0,0]]},{\"start\":16,\"stop\":23,\"col\":[[255,255,255]]},{\"start\":23,\"stop\":34,\"col\":[[0,0,255]]},{\"start\":34,\"stop\":41,\"col\":[[255,255,255]]},{\"start\":41,\"stop\":52,\"col\":[[0,0,255]]},{\"start\":52,\"stop\":57,\"col\":[[255,255,255]]},{\"start\":57,\"stop\":68,\"col\":[[255,0,0]]},{\"start\":68,\"stop\":101,\"col\":[[255,255,255]]}]}","{\"success\":true}")
setLampState :: String -> StatePatch -> IO (ByteString, ByteString)
setLampState wledUrl patch =
    let body = encode patch
    in do
      req <- parseRequest wledUrl
      res <- httpBS $ setRequestBodyJSON patch req { method = "POST", path = "json/state" }
      pure (toStrict body, getResponseBody res)

