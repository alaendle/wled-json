
{-|
Module      : WLED.Octocat.Flags
Copyright   : (c) Andreas Ländle, 2024
License     : BSD-3
Stability   : experimental

Octolamp specific feature to display flags.
-}

module WLED.Octocat.Flags(belgium, cameroon, chad, france, guatemala, guinea, ireland, italy, ivoryCoast, mali, nigeria, peru) where

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
threeVerticalStripes leftColor middleColor rightColor = (mempty :: StatePatch) { stateSeg = Just [
    segment 0 3 middleColor,
    segment 3 17 rightColor,
    segment 17 21 middleColor,
    segment 21 36 leftColor,
    segment 36 40 middleColor,
    segment 40 53 leftColor,
    segment 53 57 middleColor,
    segment 57 69 rightColor,
    segment 69 75 middleColor,
    segment 75 80 rightColor,
    segment 80 84 middleColor,
    segment 84 91 leftColor,
    segment 91 94 middleColor,
    segment 94 98 leftColor,
    segment 98 101 middleColor ]}
--threeVerticalStripes leftColor middleColor rightColor = (mempty :: StatePatch) { stateSeg = Just [ segment 0 5 middleColor, segment 5 16 rightColor, segment 16 23 middleColor, segment 23 34 leftColor, segment 34 41 middleColor, segment 41 52 leftColor, segment 52 57 middleColor, segment 57 68 rightColor, segment 68 101 middleColor ]}

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
