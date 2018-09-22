{-# LANGUAGE OverloadedStrings #-}

-- An example with higher-level Haskell functions. Haskell functions are
-- wrapped by hslua automatically for ensuring argument types and nubmers
-- and passing arguments from Lua stack to Haskell functions. Return values
-- are also handled by hslua automatically(so you don't put return value to
-- stack manually).

import qualified Data.ByteString as B
import Data.Monoid
import Foreign.Lua as Lua

main :: IO ()
main = Lua.run $ do
  openlibs
  registerHaskellFunction "concat" concat'
  registerHaskellFunction "pow" pow
  registerHaskellFunction "helloWorld" helloWorld
  loadfile "examples/haskellfun/haskellfun.lua"
  call 0 0

concat' :: B.ByteString -> B.ByteString -> Lua B.ByteString
concat' s1 s2 = return $ s1 <> s2

pow :: Lua.Number -> Lua.Number -> Lua Lua.Number
pow d1 d2 = return $ d1 ** d2

helloWorld :: Lua B.ByteString
helloWorld = return "Hello, World!"
