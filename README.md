# wled-json ✨

Haskell bindings for controlling WLED devices, with a focus on Octolamps

<p align="center">
  <img src="./images/octolamp.jpg" alt="Octolamp showing french flag" width="50%"/>
</p>

## Introduction 💡

This Haskell library provides a convenient interface for interacting with WLED devices, particularly those based on the popular Octolamp design. It leverages the WLED API (https://kno.wled.ge/) to allow you to control various aspects of your LED lights, including:

- **Color:** Set the color of your lights using various color spaces (RGB, HSV, etc.)
- **Brightness:** Adjust the overall brightness of your lights

## Installation ⚙️

```bash
# Using cabal
cabal add wled-json

# Using stack
stack add wled-json
```

## Usage 📲

```haskell
import           Control.Concurrent
import           WLED.Device
import           WLED.Octocat.Flags (france)
import           WLED.Types

main :: IO ()
main = do
    -- Connect to a WLED device
    lampState <- getLampState wledUrl
    case lampState of
        Left errMsg -> putStrLn errMsg
        Right initialState -> do
            -- Display the French flag on an Octolamp
            _ <- setLampState wledUrl france

            -- Just sleep one second
            threadDelay 1000000
            
            -- Restore the initial state
            Right currentState <- getLampState wledUrl
            _ <- setLampState wledUrl (diff currentState initialState)
            pure ()
  where
    wledUrl = "http://192.168.178.34"

```

## Features 👍

- Comprehensive WLED API coverage
- Type-safe interactions
- Easy-to-use API
- Specific support for Octolamps

## Contributing 🤝

Contributions are welcome! Please feel free to open an issue or submit a pull request.

## License ✌️

[BSD-3-Clause](./LICENSE)
