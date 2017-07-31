module LibArith where

import Foreign.C.Types (CInt(CInt))
import Foreign.Lua

foreign export ccall
  add :: LuaState -> IO NumResults

add :: LuaState -> IO NumResults
add l = runLuaWith l $ do
  i1 <- peek 1
  i2 <- peek 2
  push (i1 + i2 :: LuaNumber)
  return 1
