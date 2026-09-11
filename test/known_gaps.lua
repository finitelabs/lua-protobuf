-- Vectors that do not agree with the reference implementation today, keyed
-- "<vector> | <direction>" and valued with the ticket that tracks the defect.
--
-- Shared by test/wire_vectors_test.lua and tools/dump_wire_encodings.lua so the
-- Lua suite and `make check-wire-vectors` cannot disagree about what is known.
--
-- This is the failure_list_<lang>.txt idea from the upstream conformance suite:
-- a machine-checked version of CLAUDE.md's "What is not implemented", which
-- shrinks as gaps close rather than drifting out of date.

return {
  -- Must fail. A listed case that starts passing fails the run, so an entry can
  -- only be removed in the change that fixes the defect.
  strict = {
    ["negative sfixed32 | decode"] = "FL-18",
    ["negative sfixed32 | encode"] = "FL-18",
    ["negative sfixed32, repeated | decode"] = "FL-18",
    ["negative sfixed32, repeated | encode"] = "FL-18",
    ["map with negative sfixed32 key | decode"] = "FL-18",
    ["map with negative sfixed32 key | encode"] = "FL-18",
  },

  -- May pass or fail depending on the interpreter's number model, so neither
  -- outcome fails the run, and the result is printed rather than asserted.
  --
  -- Empty since FL-19. Its six entries were the only ones: a negative int32 or
  -- enum decoded correctly on 5.3 and 5.4, where signed 64-bit arithmetic wraps,
  -- and to 1.8446744073709552e19 on 5.1, 5.2 and LuaJIT, where it does not, so a
  -- strict entry would only have moved which half of the matrix was red. The
  -- decoder now truncates to 32 bits explicitly and the vectors are asserted on
  -- every interpreter.
  version_dependent = {},
}
