--- Shared harness for the `test/*_test.lua` modules.
---
--- `run_tests.sh` discovers modules by filename and reads only the exit code, so
--- this is the whole contract a module has to keep: build a reporter, record
--- failures as they happen, and call `finish`. Nothing else decides how a module
--- reports or how it exits.
---
--- Failures are capped rather than unbounded. A codec defect can fail thousands
--- of comparisons, and the first twenty name it as well as ten thousand do.
---
--- @class TestReporter
--- @field name string Module name, used as the prefix on every line it prints.
--- @field checked integer Comparisons run so far.

local M = {}

--- Failures printed in full before the rest are counted. One value for every
--- module: the two suites had disagreed on this (10 and 20) for no reason
--- either of them stated.
local MAX_REPORTED = 20

local Reporter = {}
Reporter.__index = Reporter

--- Creates a reporter for one test module.
--- @param name string Module name, e.g. "Wire vectors".
--- @return TestReporter reporter
function M.new(name)
  return setmetatable({
    name = name,
    checked = 0,
    failures = {},
    overflow = 0,
    notes = {},
  }, Reporter)
end

--- Counts a comparison. Independent of pass or fail, so the summary can report
--- how much was actually exercised rather than only what broke.
--- @param n integer|nil How many to add, default 1.
function Reporter:count(n)
  self.checked = self.checked + (n or 1)
end

--- Records a failure. The run fails at `finish`, not here, so one bad case does
--- not hide every case after it.
--- @param detail string
function Reporter:record(detail)
  if #self.failures < MAX_REPORTED then
    self.failures[#self.failures + 1] = detail
  else
    self.overflow = self.overflow + 1
  end
end

--- Records something worth printing that must not fail the run — a known gap
--- whose outcome depends on the interpreter, for instance. Keeping these on
--- stdout is what stops an exclusion from becoming invisible.
--- @param detail string
function Reporter:note(detail)
  self.notes[#self.notes + 1] = detail
end

--- Fails the run immediately, for a setup problem that makes the module's
--- assertions meaningless rather than for a comparison that came out wrong.
--- @param detail string
function Reporter:abort(detail)
  print(string.format("%s: FAILED, %s", self.name, detail))
  os.exit(1)
end

--- Prints the summary, the notes and the failures, then exits 0 or 1.
--- Does not return.
--- @param summary string What ran, printed whether or not anything failed.
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

--- Equality that separates the two zeros and treats NaN as equal to itself.
--- `==` does neither, and both matter to a float codec: `-0.0 == 0.0` is true on
--- the wire but not in the bytes, and `nan == nan` is false.
--- @param a any
--- @param b any
--- @return boolean equal
function M.same_number(a, b)
  if a ~= a or b ~= b then
    return a ~= a and b ~= b
  end
  if a == 0 and b == 0 then
    return (1 / a) == (1 / b)
  end
  return a == b
end

--- Resolves the directory holding the calling test file, so a module loads its
--- fixtures the same way whether it is run directly or by `run_tests.sh`.
--- @param level integer|nil Stack level of the caller, default 2.
--- @return string dir Path with a trailing separator.
function M.script_dir(level)
  return debug.getinfo(level or 2, "S").source:match("^@(.*[/\\])") or "./"
end

return M
