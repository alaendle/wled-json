
{-|
Module      : WLED.Octocat.Flags
Copyright   : (c) Andreas Ländle, 2024-2025
License     : BSD-3
Stability   : experimental

Octolamp specific feature to display flags.
-}

module WLED.Octocat.Flags(belgium, cameroon, chad, france, guatemala, guinea, ireland, italy, ivoryCoast, mali, nigeria, peru) where

import           Data.List            (group)
import           WLED.Device          (DeviceSpec (DeviceSpec))
import           WLED.Octocat.Octocat (deviceSpec)
import           WLED.Types

-- | The Belgian flag.
belgium :: StatePatch
belgium = threeVerticalStripes ral9017 ral2007 ral3028

-- | The Cameroonian flag.
cameroon :: StatePatch
cameroon = threeVerticalStripes ral6024 ral3028 ral1018

-- | The Chad flag.
chad :: StatePatch
chad = threeVerticalStripes ral5026 ral2007 ral3028

-- | The French flag.
france :: StatePatch
france = threeVerticalStripes ral5002 white ral3020

-- | The Guatemalan flag.
guatemala :: StatePatch
guatemala = threeVerticalStripes ral5012 white ral5012

-- | The Guinean flag.
guinea :: StatePatch
guinea = threeVerticalStripes ral3028 ral1018 ral6024

-- | The Irish flag.
ireland :: StatePatch
ireland = threeVerticalStripes ral6024 white ral1028

-- | The Italian flag.
italy :: StatePatch
italy = threeVerticalStripes ral6024 ral9016 ral3028

-- | The Ivory Coast flag.
ivoryCoast :: StatePatch
ivoryCoast = threeVerticalStripes ral1028 white ral6024

-- | The Malian flag.
mali :: StatePatch
mali = threeVerticalStripes ral6038 ral1018 ral3028

-- | The Nigerian flag.
nigeria :: StatePatch
nigeria = threeVerticalStripes ral6001 white ral6001

-- | The Peruvian flag.
peru :: StatePatch
peru = threeVerticalStripes ral3028 white ral3028

threeVerticalStripes :: [Int] -> [Int] -> [Int] -> StatePatch
threeVerticalStripes leftColor middleColor rightColor = (mempty :: StatePatch) { stateSeg = segments } where
        segments = Just $ (\(start, stop, index) -> segment start stop ([leftColor, middleColor, rightColor] !! index)) <$> buildSegments (verticalLayer deviceSpec 3)

-- >>> length <$> (group $ horizontalLayer (DeviceSpec lightPositions (100,100)) 3)
-- [8,4,15,4,14,4,12,4,8,6,7,6,9]
horizontalLayer :: Real a => DeviceSpec a -> Int -> [Int]
horizontalLayer (DeviceSpec pos (_, sy)) n = fmap (\(_, y) -> floor (toRational  y / toRational sy * toRational n)) pos

-- >>> length <$> (group $ verticalLayer (DeviceSpec lightPositions (100,100)) 3)
-- [3,14,4,15,4,13,4,12,6,5,4,7,3,4,3]
verticalLayer :: Real a => DeviceSpec a -> Int -> [Int]
verticalLayer (DeviceSpec pos (sx, _)) n = fmap (\(x, _) -> floor (toRational  x / toRational sx * toRational n)) pos

-- >>> buildSegments $ verticalLayer (DeviceSpec lightPositions (100,100)) 3
-- [(0,3,1),(3,17,2),(17,21,1),(21,36,0),(36,40,1),(40,53,0),(53,57,1),(57,69,2),(69,75,1),(75,80,2),(80,84,1),(84,91,0),(91,94,1),(94,98,0),(98,101,1)]
buildSegments :: [Int] -> [(Int, Int, Int)]
buildSegments =  step 0 . group where
    step :: Int -> [[Int]] -> [(Int, Int, Int)]
    step n (xs@(x:_):r) = (n, n + length xs, x) : step (n + length xs) r
    step _ _            = []


ral1018 :: [Int]
ral1018 = [252, 209, 22]

ral1028 :: [Int]
ral1028 = [255, 120, 0]

ral2007 :: [Int]
ral2007 = [255, 205, 0]

ral3020 :: [Int]
ral3020 = [255,0,15]

ral3028 :: [Int]
ral3028 = [200, 16, 46]

ral5002 :: [Int]
ral5002 = [0,0,145]

ral5012 :: [Int]
ral5012 = [73, 151, 208]

ral5026 :: [Int]
ral5026 = [0, 38, 100]

ral6001 :: [Int]
ral6001 = [27, 115, 57]

ral6024 :: [Int]
ral6024 = [0, 154, 68]

ral6038 :: [Int]
ral6038 = [20, 181, 58]

ral9016 :: [Int]
ral9016 = [244, 249, 255]

ral9017 :: [Int]
ral9017 = [45, 41, 28]

white :: [Int]
white = [255,255,255]
