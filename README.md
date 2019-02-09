# tdlib-haskell-bindings

[Telegram Database Library](https://github.com/tdlib/td#using-json) (TDLib) haskell bindings example. Examples in other languages can be found [here](https://github.com/tdlib/td/tree/master/example)

This example considers prebuilt dynamic `libtdjson.[so|dylib|dll]` in `lib` folder.

# To execute:

1. [Build tdlib](https://core.telegram.org/tdlib/docs/#building) or get precompiled binaries.

2. `stack ghci --ghci-options -ltdjson`

# Notes

This may be useful: https://github.com/Bannerets/tdlib-binaries

The copy of build instructions (they are a bit different) can be also found here: https://github.com/tdlib/td#building
