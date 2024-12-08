{-# LANGUAGE Arrows           #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators    #-}
#if __GLASGOW_HASKELL__ < 904
{-# LANGUAGE TypeFamilies     #-}
#endif

module Main (main) where

import           Control.Monad      (void)
import           FRP.Rhine
import           System.Environment (getArgs)
import           System.IO          (BufferMode (NoBuffering), hSetBuffering, stdout)
import           WLED.Device
import           WLED.Octocat.Flags (belgium, cameroon, chad, france, guatemala, guinea, ireland, italy, ivoryCoast, mali, nigeria, peru)
import           WLED.Types

waitForEnter :: ClSF (ExceptT () IO) StdinClock () ()
waitForEnter = arrMCl throwE

sinusWave :: Monad m => Int -> Int -> Double -> ClSF m cl Double Int
sinusWave low high frequency = arr scaledSinusWave
  where
    timeToSinusWave :: Double -> Double
    timeToSinusWave t = sin (2 * pi * frequency * t)
    scaledSinusWave :: Double -> Int
    scaledSinusWave time = round $ fromIntegral (high - low) / 2 * timeToSinusWave time + fromIntegral (high + low) / 2

brightnessSinus :: Monad m => Double -> ClSF m cl Double StatePatch
brightnessSinus frequency = sinusWave 0 255 frequency >-> arr (\bri -> (mempty :: StatePatch) { stateBri = Just bri })

switchFlags :: Monad m => Double -> ClSF m cl Double StatePatch
switchFlags frequency = arr (\t -> allFlags !! (floor (t * frequency) `mod` length allFlags)) where
  allFlags = [belgium, cameroon, chad, france, guatemala, guinea, ireland, italy, ivoryCoast, mali, nigeria, peru]

animation :: (Monad m, TimeDomain (Time cl), Diff (Time cl) ~ Double) => ClSF m cl () StatePatch
animation = sinceStart >-> proc time -> do
  brightness <- brightnessSinus 0.1 -< time
  flag <- switchFlags 0.2 -< time
  returnA -< brightness <> flag

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "-----------------------------------------------------------------------"
  putStrLn "Welcome to the wled-json demo."
  putStrLn "-----------------------------------------------------------------------"
  args <- getArgs
  mainLoop $ if null args then defaultWledUrl else head args
  where
    mainLoop :: String -> IO ()
    mainLoop wledUrl = do
      putStrLn ""
      putStrLn "1: Change URL for WLED device."
      putStrLn "2: Run octolamp flags demo."
      putStrLn "q: Quit."
      putStrLn ""
      putStr $ "(" ++ wledUrl ++ ") > "
      choice <- getLine
      case choice of
        "1" -> do
          putStrLn ""
          putStr "Enter new URL: "
          getLine >>= mainLoop
        "2" -> do
          putStrLn ""
          putStrLn "Press [Enter] to stop demonstration."
          lampState <- getLampState wledUrl
          case lampState of
            Left errMsg -> putStrLn errMsg
            Right initialState -> do
              void $ runExceptT $ flow $ waitForEnter @@ StdinClock |@| (animation >-> arrMCl (liftIO . void . setLampState wledUrl)) @@ liftClock @IO @(ExceptT ()) (waitClock @100)
              Right currentState <- getLampState wledUrl
              _ <- setLampState wledUrl (diff currentState initialState)
              -- validate that initial state is restored
              Right restoredState <- getLampState wledUrl
              putStrLn $ "Initial state is restored: " <> show (initialState == restoredState)
          mainLoop wledUrl
        "q" -> pure ()
        "Q" -> pure ()
        _ -> do
          putStrLn "Could not recognize option."
          mainLoop wledUrl
    defaultWledUrl :: String
    defaultWledUrl = "http://192.168.178.34"
