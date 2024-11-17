{-# LANGUAGE Arrows                   #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module Main (main) where

import           Control.Monad (void)
import           Data.Kind     (Type)
import           FRP.Rhine
import           GHC.TypeLits  (Nat)
import           Lib
import           Octocat       (france)
import           Types

waitForEnter :: ClSF (ExceptT () IO) StdinClock () ()
waitForEnter = arrMCl throwE

type TimeClock :: Nat -> Type
type TimeClock ms = LiftClock IO (ExceptT ()) (Millisecond ms)

sinusWave :: (Int -> ExceptT () IO ()) -> ClSF (ExceptT () IO) (TimeClock 100) () ()
sinusWave signalHandler = sinceStart >>> proc time -> do
    arrMCl (signalHandler . scaledSinusWave 0 255 0.1) -< time
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
        Right initialState -> do
            _ <- setLampState wledUrl france
            void $ runExceptT $ flow $ waitForEnter @@ StdinClock |@| sinusWave (\bri -> liftIO $ void $ setLampState wledUrl (mempty :: StatePatch) { stateBri = Just bri }) @@ liftClock waitClock
            Right currentState <- getLampState wledUrl
            _ <- setLampState wledUrl (diff currentState initialState)
            -- validate that initial state is restored
            Right restoredState <- getLampState wledUrl
            putStrLn $ "Initial state is restored: " <> show (initialState == restoredState)
  where
    wledUrl = "http://192.168.178.34"
