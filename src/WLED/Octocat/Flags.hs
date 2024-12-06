
{-|
Module      : WLED.Octocat.Flags
Copyright   : (c) Andreas Ländle, 2024
License     : BSD-3
Stability   : experimental

Octolamp specific feature to display flags.
-}

module WLED.Octocat.Flags(france, guatemala, nigeria, peru) where

import           WLED.Types

-- | The French flag.
-- >>> france
-- State {stateOn = Nothing, stateBri = Nothing, stateTransition = Nothing, statePs = Nothing, statePl = Nothing, stateNl = Nothing, stateLor = Nothing, stateMainseg = Nothing, stateSeg = Just [Segment {segmentId = Nothing, segmentStart = Just 0, segmentStop = Just 5, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,255,255]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 5, segmentStop = Just 16, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,0,15]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 16, segmentStop = Just 23, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,255,255]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 23, segmentStop = Just 34, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[0,0,145]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 34, segmentStop = Just 41, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,255,255]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 41, segmentStop = Just 52, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[0,0,145]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 52, segmentStop = Just 57, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,255,255]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 57, segmentStop = Just 68, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,0,15]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing},Segment {segmentId = Nothing, segmentStart = Just 68, segmentStop = Just 101, segmentLen = Nothing, segmentGrp = Nothing, segmentSpc = Nothing, segmentOf = Nothing, segmentOn = Nothing, segmentFrz = Nothing, segmentBri = Nothing, segmentCct = Nothing, segmentSet = Nothing, segmentCol = Just [[255,255,255]], segmentFx = Just 0, segmentSx = Nothing, segmentIx = Nothing, segmentPal = Nothing, segmentC1 = Nothing, segmentC2 = Nothing, segmentC3 = Nothing, segmentSel = Nothing, segmentRev = Nothing, segmentMi = Nothing, segmentO1 = Nothing, segmentO2 = Nothing, segmentO3 = Nothing, segmentSi = Nothing, segmentM12 = Nothing}]}
france :: StatePatch
france = threeVerticalStripes blue white red where
    red :: [Int]
    red = [255,0,15]
    blue :: [Int]
    blue = [0,0,145]

-- | The Guatemalan flag.
guatemala :: StatePatch
guatemala = threeVerticalStripes blue white blue where
    blue :: [Int]
    blue = [73, 151, 208]

-- | The Nigerian flag.
nigeria :: StatePatch
nigeria = threeVerticalStripes green white green where
    green :: [Int]
    green = [27, 115, 57]

-- | The Peruvian flag.
peru :: StatePatch
peru = threeVerticalStripes red white red where
    red :: [Int]
    red = [200, 16, 46]

threeVerticalStripes :: [Int] -> [Int] -> [Int] -> StatePatch
threeVerticalStripes leftColor middleColor rightColor = (mempty :: StatePatch) { stateSeg = Just [ seg 0 5 middleColor, seg 5 16 rightColor, seg 16 23 middleColor, seg 23 34 leftColor, seg 34 41 middleColor, seg 41 52 leftColor, seg 52 57 middleColor, seg 57 68 rightColor, seg 68 101 middleColor ]}

seg :: Int -> Int -> [Int] -> SegmentPatch
seg start stop color = (mempty :: SegmentPatch) { segmentStart = Just start, segmentStop = Just stop, segmentCol = Just [color], segmentFx = Just 0 }

white :: [Int]
white = [255,255,255]
