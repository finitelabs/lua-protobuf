-- @test-name Protobuf operations under lpack
-- @test-modes native
--
-- Control4's LuaJIT has string.pack/unpack as lpack, a different dialect from 5.3.
-- The vendored bitn must decline it and take the byte-math path.

local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
dofile(here .. "lpack_stub.lua")

os.exit(dofile(here .. "test_protobuf.lua")() and 0 or 1)
