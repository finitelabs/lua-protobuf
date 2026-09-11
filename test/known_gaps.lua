-- Vectors that disagree with the reference implementation today, keyed
-- "<vector> | <direction>" and valued with the tracking ticket.

return {
  -- Must fail: a listed case that passes fails the run. Remove the entry in the fix.
  strict = {
    ["negative sfixed32 | decode"] = "FL-18",
    ["negative sfixed32 | encode"] = "FL-18",
    ["negative sfixed32, repeated | decode"] = "FL-18",
    ["negative sfixed32, repeated | encode"] = "FL-18",
    ["map with negative sfixed32 key | decode"] = "FL-18",
    ["map with negative sfixed32 key | encode"] = "FL-18",
  },

  -- Outcome depends on the interpreter's number model: printed, never asserted.
  version_dependent = {
    ["negative scalars | decode"] = "FL-19",
    ["negative scalars | encode"] = "FL-19",
    ["negative enum | decode"] = "FL-19",
    ["negative enum | encode"] = "FL-19",
    ["maps with 32-bit keys | decode"] = "FL-19",
    ["maps with 32-bit keys | encode"] = "FL-19",
  },
}
