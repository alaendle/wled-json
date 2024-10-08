module Main (main) where

import Lib
import Types

main :: IO ()
main = do
    lampState <- getLampState wledUrl
    case lampState of
        Left errMsg -> putStrLn errMsg
        Right state -> do
            _ <- setLampState wledUrl (mempty :: StatePatch) { stateOn = Just (not (stateOn state)) }
            pure ()
  where
    wledUrl = "http://192.168.178.34"
