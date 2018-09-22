function fail_when_zero(n)
  print("Lua: " .. tostring(n))
  if n == 0 then
    error("Failing from Lua")
  end
  return fail_when_zero_haskell(n - 1)
end
