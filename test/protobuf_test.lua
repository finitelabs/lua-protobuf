-- @test-name Protobuf operations
--
-- selftest() stays in src/ so it ships and can run on a controller; this only wraps it.

local pb = require("protobuf")

os.exit(pb.selftest() and 0 or 1)
