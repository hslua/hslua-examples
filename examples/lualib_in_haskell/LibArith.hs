module LibArith where

import Foreign.C.Types (CInt (CInt))
import Foreign.Lua as Lua

foreign export ccall
  add :: Lua.State -> IO NumResults

add :: Lua.State -> IO NumResults
add l = runWith l $ do
  i1 <- peek 1
  i2 <- peek 2
  push (i1 + i2 :: Lua.Number)
  return 1
