--- Main chunk of build/protobuf-tests.lua, the with-tests build: the library, as `require("protobuf")`
--- returns it, plus `selftest()` running test/test_protobuf.lua.

--- @class protobuf.with_tests: protobuf
local pb = require("protobuf")
local test_protobuf = require("test_protobuf")

--- Run the protobuf test vectors.
--- @return boolean success True if all tests passed
function pb.selftest()
  return test_protobuf()
end

return pb
