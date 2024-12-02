
{-|
Module      : WLED.Octocat.Flags
Copyright   : (c) Andreas Ländle, 2024
License     : BSD-3
Stability   : experimental

Octolamp specific feature to display flags.
-}

module WLED.Octocat.Flags(france) where

import           WLED.Types

-- | The French flag.
france :: StatePatch
france = (mempty :: StatePatch) { stateSeg = Just [ seg 0 5 white, seg 5 16 red, seg 16 23 white, seg 23 34 blue, seg 34 41 white, seg 41 52 blue, seg 52 57 white, seg 57 68 red, seg 68 101 white ]} where
    red :: [Int]
    red = [255,0,0]
    white :: [Int]
    white = [255,255,255]
    blue :: [Int]
    blue = [0,0,255]

seg :: Int -> Int -> [Int] -> SegmentPatch
seg start stop color = (mempty :: SegmentPatch) { segmentStart = Just start, segmentStop = Just stop, segmentCol = Just [color], segmentFx = Just 0 }
