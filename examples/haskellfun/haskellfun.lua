function catch_haskell(f, ...)
  local ok, ret
  ok, ret = pcall(f, ...)
  if not ok then
    print("Error caught from Haskell land: " .. ret)
    return
  end
  return ret
end

print(concat("hello", " world!"))
print(catch_haskell(pow, 3.2, 5))
print(catch_haskell(helloWorld))
print(catch_haskell(pow, "wrong"))
print(catch_haskell(pow, 3))
