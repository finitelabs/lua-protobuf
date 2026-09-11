--- Shared harness for the `test/*_test.lua` modules.
---
--- @class TestReporter
--- @field name string
--- @field checked integer

local M = {}

local MAX_REPORTED = 20

local Reporter = {}
Reporter.__index = Reporter

--- @param name string
--- @return TestReporter
function M.new(name)
  return setmetatable({
    name = name,
    checked = 0,
    failures = {},
    overflow = 0,
    notes = {},
  }, Reporter)
end

--- @param n integer|nil Defaults to 1.
function Reporter:count(n)
  self.checked = self.checked + (n or 1)
end

--- Records a failure; the run fails at `finish`, not here.
--- @param detail string
function Reporter:record(detail)
  if #self.failures < MAX_REPORTED then
    self.failures[#self.failures + 1] = detail
  else
    self.overflow = self.overflow + 1
  end
end

--- Printed at `finish`; never fails the run.
--- @param detail string
function Reporter:note(detail)
  self.notes[#self.notes + 1] = detail
end

--- Exits 1 immediately, for setup failures.
--- @param detail string
function Reporter:abort(detail)
  print(string.format("%s: FAILED, %s", self.name, detail))
  os.exit(1)
end

--- Prints the report and exits 0 or 1. Does not return.
--- @param summary string Printed always.
--- @param success string Printed only when nothing failed.
function Reporter:finish(summary, success)
  print(string.format("%s: %s", self.name, summary))

  for _, detail in ipairs(self.notes) do
    print("  " .. detail)
  end

  for _, detail in ipairs(self.failures) do
    print("  FAIL: " .. detail)
  end
  if self.overflow > 0 then
    print(string.format("  FAIL: and %d further mismatches", self.overflow))
  end

  if #self.failures > 0 then
    print(self.name .. ": FAILED")
    os.exit(1)
  end

  print(string.format("%s: %s", self.name, success))
  os.exit(0)
end

--- `==`, except that -0.0 and 0.0 differ and NaN equals NaN.
--- @param a any
--- @param b any
--- @return boolean
function M.same_number(a, b)
  if a ~= a or b ~= b then
    return a ~= a and b ~= b
  end
  if a == 0 and b == 0 then
    return (1 / a) == (1 / b)
  end
  return a == b
end

return M
