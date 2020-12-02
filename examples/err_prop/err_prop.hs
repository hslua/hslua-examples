{-# LANGUAGE OverloadedStrings #-}

-- An example program that demonstrates error propagation between Haskell and
-- Lua. It creates a function call stack with 10 functions, like this:
--
--   Lua function
--   ...
--   Haskell function
--   Lua function
--   Program
--
-- And then the function at the top throws an error, according to the error
-- conventions described in the docs. The error is propagated to the program at
-- the bottom.
--
-- Then the same thing happens, starting with Haskell function:
--
--   Haskell function
--   ...
--   Lua function
--   Haskell function
--   Program

import qualified Data.ByteString.Char8 as BC
import Foreign.C.Types (CInt)
import Foreign.Lua as Lua

main :: IO ()
main = run $ do
  openlibs
  registerHaskellFunction "fail_when_zero_haskell" failWhenZero

  -- Define the Lua function
  loadfile "examples/err_prop/err_prop.lua"
  call 0 0

  -- Start the loop by calling Lua function with argument 10
  getglobal "fail_when_zero"
  pushinteger 10
  -- Since Lua function will be the one that propagates error to the program,
  -- we need to catch it using `pcall`
  ret <- pcall 1 1 Nothing
  errMsg <- peek 1
  liftIO $ putStrLn $ "ret: " ++ show ret
  liftIO $ putStrLn $ "errMsg: " ++ errMsg

  top <- gettop
  liftIO $ putStrLn $ "top: " ++ show top
  pop 1

  -- start the loop by calling Haskell function with argument 10
  getglobal "fail_when_zero_haskell"
  pushinteger 10
  -- Our convention is that Haskell functions never use `lua_error` because
  -- it's never safe(it's not even exported by the library for this reason).
  -- So if we're calling a Haskell function that `pcall` and `call` does the
  -- same thing.
  _ <- pcall 1 2 Nothing
  -- We know it failed, so just read the error message without checking
  -- first argument
  errMsg <- peek 1
  liftIO $ putStrLn $ "errMsg: " ++ errMsg
  pop 2

failWhenZero :: Lua NumResults
failWhenZero = do
  i <- peek 1 :: Lua Lua.Integer
  liftIO $ putStrLn $ "Haskell: " ++ show i
  if i == 0
    then pushstring "Failing from Haskell" *> Lua.error
    else do
      getglobal "fail_when_zero"
      pushinteger (i - 1)
      ret <- pcall 1 1 Nothing
      if ret /= OK
        then
          -- propagate the error. no need to push error message since it's
          -- already at the top of the stack at this point. (because of how
          -- `pcall` works)
          Lua.error
        else
          -- Lua function's return value is on the stack, return it
          return 1
