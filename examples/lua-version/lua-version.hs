{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Foreign.Lua as Lua

main :: IO ()
main = Lua.run $ do
  Lua.openlibs
  Lua.getglobal "print"
  Lua.pushstring "Hello from"
  Lua.getglobal "_VERSION"
  Lua.call 2 0
