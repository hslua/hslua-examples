HsLua examples
==============

**This repository has been archived. See the `hslua-examples` folder in the
main hslua repo for up-to-date examples.**

The following examples are available:

- **lua-version**: A simple program which uses Lua library functions and
  Lua variables to print the Lua version against which the program was
  linked.

- **haskellfun**: Demo how functions written in Haskell can be exposed
  to Lua. Includes a short Lua script which makes use of these
  functions.

- **callbacks**: Program that demonstrates how Haskell callbacks can be
  passed to Lua, and how Lua callbacks can be collected and called from
  Haskell.

- **err_prop**: Demonstrates how errors propagate in HsLua programs.
  This consists of two parts: the Haskell program, and a short Lua
  script.

- **lualib_in_haskell**: Lua can make use of dynamically loaded
  libraries. This shows how such a library can be created with HsLua,
  exposing functions written in Haskell to Lua.
