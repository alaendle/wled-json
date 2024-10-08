{-# LANGUAGE Arrows #-}
{-# LANGUAGE DataKinds #-}

module Main (main) where

import Lib
import Types

import FRP.Rhine
import Control.Monad (void)

waitForEnter :: ClSF (ExceptT () IO) StdinClock () ()
waitForEnter = arrMCl throwE

type TimeClock ms = LiftClock IO (ExceptT ()) (Millisecond ms)

sinusWave :: (Int -> ExceptT () IO ()) -> ClSF (ExceptT () IO) (TimeClock 100) () ()
sinusWave signalHandler = sinceStart >>> proc time -> do
    arrMCl (signalHandler . scaledSinusWave 0 31 0.5) -< time
    returnA -< ()
  where
    timeToSinusWave :: Double -> Double -> Double
    timeToSinusWave frequency t = sin (2 * pi * frequency * t)
    scaledSinusWave :: Int -> Int -> Double -> Double -> Int
    scaledSinusWave a b frequency time = round $ fromIntegral (b - a) / 2 * timeToSinusWave frequency time + fromIntegral (a + b) / 2

main :: IO ()
main = do
    lampState <- getLampState wledUrl
    case lampState of
        Left errMsg -> putStrLn errMsg
        Right _ -> do
            void $ runExceptT $ flow $ waitForEnter @@ StdinClock |@| sinusWave (\bri -> liftIO $ void $ setLampState wledUrl (mempty :: StatePatch) { stateBri = Just bri }) @@ liftClock waitClock
  where
    wledUrl = "http://192.168.178.34"
