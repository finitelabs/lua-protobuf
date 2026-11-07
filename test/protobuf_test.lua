-- @test-name Protobuf operations
--
-- Runs the suite in test_protobuf.lua, which the with-tests build also carries.

local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"

os.exit(dofile(here .. "test_protobuf.lua")() and 0 or 1)
