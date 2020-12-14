# string

[![Hackage](https://img.shields.io/hackage/v/string.svg)](https://hackage.haskell.org/package/string)
[![BSD3 license](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Build Status](https://github.com/chessai/string/workflows/ci/badge.svg)](https://github.com/chessai/string/actions?query=workflow%3Aci)

String library for Haskell with sensible defaults
  - UTF-8
  - strict only (no lazy String abstraction, this should be done with a proper streaming abstraction, not a linked list)
  - no stream fusion (predictable performance)
  - rich Builder API
  - control over pinnedness 
  - seamless FFI integration
