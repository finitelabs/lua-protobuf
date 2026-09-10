-- @test-name Protobuf operations
--
-- The embedded suite lives in src/protobuf/init.lua so that it ships inside the
-- amalgamated module and can be run on a controller, against the real LuaJIT,
-- from a driver. That placement is deliberate and stays.
--
-- This wrapper exists only so the embedded suite is discovered and reports its
-- result the same way as every other module here, rather than needing a special
-- case in run_tests.sh. It owns its own output, so it does not use testlib.

local pb = require("protobuf")

os.exit(pb.selftest() and 0 or 1)
