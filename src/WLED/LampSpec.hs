{-# LANGUAGE StandaloneKindSignatures #-}

{-|
Module      : WLED.LampSpec
Copyright   : (c) Andreas Ländle, 2024-2026
License     : BSD-3
Stability   : experimental

Represent the logical model of the lamp.
-}

module WLED.LampSpec (LampSpec(..)) where

import           Data.Kind (Type)

type LampSpec :: Type -> Type
data LampSpec a = LampSpec { positions :: [(a, a)], size :: (a, a) }
