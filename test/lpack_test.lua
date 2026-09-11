-- @test-name Selftest under lpack
-- @test-modes native
--
-- Control4's LuaJIT has string.pack/unpack as lpack, a different dialect from 5.3.
-- The vendored bitn must decline it and take the byte-math path.

local here = debug.getinfo(1, "S").source:match("^@(.*[/\\])") or "./"
dofile(here .. "lpack_stub.lua")

local pb = require("protobuf")
os.exit(pb.selftest() and 0 or 1)
