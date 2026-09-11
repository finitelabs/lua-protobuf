-- Generated wire-format vectors. Do not edit manually.
--
-- Produced by tools/gen_wire_vectors from the reference protobuf
-- implementation against test/test_messages_proto3.proto. Regenerate with
-- `make gen-wire-vectors`; `make check-wire-vectors` fails on drift.
--
-- `golden` is the reference implementation's own serialization. `expected` is
-- derived by walking that same message, so neither side is hand-computed.

local bit64 = require("bitn").bit64
local I = bit64.new

return {
  {
    name = "singular scalars",
    note = "one value per scalar wire type",
    golden = "\008\001\016\002\024\003 \004(\0100\012=\007\000\000\000A\008\000\000\000\000\000\000\000M\009\000\000\000Q\010\000\000\000\000\000\000\000]\000\000\192?a\000\000\000\000\000\000\002@h\001r\005helloz\004\000\001\254\255",
    expected = {
      optional_int32 = 1,
      optional_int64 = I(0, 2),
      optional_uint32 = 3,
      optional_uint64 = I(0, 4),
      optional_sint32 = 5,
      optional_sint64 = I(0, 6),
      optional_fixed32 = 7,
      optional_fixed64 = I(0, 8),
      optional_sfixed32 = 9,
      optional_sfixed64 = I(0, 10),
      optional_float = 1.5,
      optional_double = 2.25,
      optional_bool = true,
      optional_string = "hello",
      optional_bytes = "\000\001\254\255",
    },
  },
  {
    name = "32-bit boundaries",
    note = "int32/uint32/fixed32 extremes",
    golden = "\008\255\255\255\255\007\024\255\255\255\255\015(\255\255\255\255\015=\255\255\255\255M\255\255\255\127",
    expected = {
      optional_int32 = 2147483647,
      optional_uint32 = 4294967295,
      optional_sint32 = -2147483648,
      optional_fixed32 = 4294967295,
      optional_sfixed32 = 2147483647,
    },
  },
  {
    name = "64-bit beyond 53 bits",
    note = "values a double cannot hold exactly",
    golden = "\016\255\255\255\255\255\255\255\255? \255\255\255\255\255\255\255\255\255\0010\255\255\255\255\255\255\255\255\127A\255\255\255\255\255\255\255\255Q\000\000\000\000\000\000\000\192",
    expected = {
      optional_int64 = I(1073741823, 4294967295),
      optional_uint64 = I(4294967295, 4294967295),
      optional_sint64 = I(3221225472, 0),
      optional_fixed64 = I(4294967295, 4294967295),
      optional_sfixed64 = I(3221225472, 0),
    },
  },
  {
    name = "negative scalars",
    note = "sign extension and zigzag",
    golden = "\008\255\255\255\255\255\255\255\255\255\001\016\255\255\255\255\255\255\255\255\255\001(\0010\001Q\255\255\255\255\255\255\255\255",
    expected = {
      optional_int32 = -1,
      optional_int64 = I(4294967295, 4294967295),
      optional_sint32 = -1,
      optional_sint64 = I(4294967295, 4294967295),
      optional_sfixed64 = I(4294967295, 4294967295),
    },
  },
  {
    name = "negative sfixed32",
    note = "sfixed32 is signed in both directions",
    golden = "M\255\255\255\255",
    expected = {
      optional_sfixed32 = -1,
    },
  },
  {
    name = "negative sfixed32, repeated",
    note = "negative sfixed32 through the packed and unpacked paths",
    golden = "\186\002\012\255\255\255\255\000\000\000\000\001\000\000\000\154\005\008\000\000\000\128\255\255\255\127\141\006\251\255\255\255",
    expected = {
      repeated_sfixed32 = { -1, 0, 1 },
      packed_sfixed32 = { -2147483648, 2147483647 },
      unpacked_sfixed32 = { -5 },
    },
  },
  {
    name = "strings and bytes",
    note = "multibyte UTF-8 and every byte value",
    golden = "r\009\195\169\228\184\173\240\159\148\146z\128\002\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255\194\001\005piece\202\001\004cord",
    expected = {
      optional_string = "\195\169\228\184\173\240\159\148\146",
      optional_bytes = "\000\001\002\003\004\005\006\007\008\009\010\011\012\013\014\015\016\017\018\019\020\021\022\023\024\025\026\027\028\029\030\031 !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\127\128\129\130\131\132\133\134\135\136\137\138\139\140\141\142\143\144\145\146\147\148\149\150\151\152\153\154\155\156\157\158\159\160\161\162\163\164\165\166\167\168\169\170\171\172\173\174\175\176\177\178\179\180\181\182\183\184\185\186\187\188\189\190\191\192\193\194\195\196\197\198\199\200\201\202\203\204\205\206\207\208\209\210\211\212\213\214\215\216\217\218\219\220\221\222\223\224\225\226\227\228\229\230\231\232\233\234\235\236\237\238\239\240\241\242\243\244\245\246\247\248\249\250\251\252\253\254\255",
      optional_string_piece = "piece",
      optional_cord = "cord",
    },
  },
  {
    name = "nested message",
    note = "singular nested and foreign messages",
    golden = "\146\001\002\008*\154\001\002\008\007",
    expected = {
      optional_nested_message = {
        a = 42,
      },
      optional_foreign_message = {
        c = 7,
      },
    },
  },
  {
    name = "corecursive nesting",
    note = "message nested three deep through NestedMessage",
    golden = "\146\001\011\008\001\018\007\008\002\146\001\002\008\003",
    expected = {
      optional_nested_message = {
        a = 1,
        corecursive = {
          optional_int32 = 2,
          optional_nested_message = {
            a = 3,
          },
        },
      },
    },
  },
  {
    name = "recursive message",
    note = "TestAllTypesProto3 inside itself",
    golden = "\218\001\007\008\010\218\001\002\008\020",
    expected = {
      recursive_message = {
        optional_int32 = 10,
        recursive_message = {
          optional_int32 = 20,
        },
      },
    },
  },
  {
    name = "present empty submessage",
    note = "presence with no fields set",
    golden = "\146\001\000",
    expected = {
      optional_nested_message = {},
    },
  },
  {
    name = "enums",
    note = "nested, foreign and aliased enums",
    golden = "\168\001\002\176\001\001\184\001\002",
    expected = {
      optional_nested_enum = 2,
      optional_foreign_enum = 1,
      optional_aliased_enum = 2,
    },
  },
  {
    name = "negative enum",
    note = "NEG = -1 sign-extends to a ten-byte varint",
    golden = "\168\001\255\255\255\255\255\255\255\255\255\001",
    expected = {
      optional_nested_enum = -1,
    },
  },
  {
    name = "repeated scalars, proto3 default",
    note = "packed by the reference implementation",
    golden = "\250\001\003\001\002\003\130\002\002\004\005\138\002\001\006\154\002\002\001\002\170\002\008\007\000\000\000\008\000\000\000\178\002\008\009\000\000\000\000\000\000\000\202\002\008\000\000\192?\000\000 \192\210\002\008\000\000\000\000\000\000\010@\218\002\003\001\000\001",
    expected = {
      repeated_int32 = { 1, 2, 3 },
      repeated_int64 = { I(0, 4), I(0, 5) },
      repeated_uint32 = { 6 },
      repeated_sint32 = { -1, 1 },
      repeated_fixed32 = { 7, 8 },
      repeated_fixed64 = { I(0, 9) },
      repeated_float = { 1.5, -2.5 },
      repeated_double = { 3.25 },
      repeated_bool = { true, false, true },
    },
  },
  {
    name = "packed repeated",
    note = "explicit [packed = true] block",
    golden = "\218\004\003\001\002\003\226\004\002\004\005\130\005\002\001\003\138\005\008\006\000\000\000\007\000\000\000\178\005\016\000\000\000\000\000\000\248?\000\000\000\000\000\000\004@\186\005\002\001\000\194\005\003\000\001\002",
    expected = {
      packed_int32 = { 1, 2, 3 },
      packed_int64 = { I(0, 4), I(0, 5) },
      packed_sint64 = { I(4294967295, 4294967295), I(4294967295, 4294967294) },
      packed_fixed32 = { 6, 7 },
      packed_double = { 1.5, 2.5 },
      packed_bool = { true, false },
      packed_nested_enum = { 0, 1, 2 },
    },
  },
  {
    name = "unpacked repeated",
    note = "explicit [packed = false], one tag per element",
    golden = "\200\005\001\200\005\002\200\005\003\208\005\004\208\005\005\240\005\001\240\005\003\253\005\006\000\000\000\253\005\007\000\000\000\161\006\000\000\000\000\000\000\248?\161\006\000\000\000\000\000\000\004@\168\006\001\168\006\000\176\006\000\176\006\001\176\006\002",
    expected = {
      unpacked_int32 = { 1, 2, 3 },
      unpacked_int64 = { I(0, 4), I(0, 5) },
      unpacked_sint64 = { I(4294967295, 4294967295), I(4294967295, 4294967294) },
      unpacked_fixed32 = { 6, 7 },
      unpacked_double = { 1.5, 2.5 },
      unpacked_bool = { true, false },
      unpacked_nested_enum = { 0, 1, 2 },
    },
  },
  {
    name = "repeated length-delimited",
    note = "strings, bytes and messages are never packed",
    golden = "\226\002\001a\226\002\000\226\002\003ccc\234\002\001\000\234\002\002\255\254\130\003\002\008\001\130\003\002\008\002\138\003\002\008\003",
    expected = {
      repeated_string = { "a", "", "ccc" },
      repeated_bytes = { "\000", "\255\254" },
      repeated_nested_message = { {
          a = 1,
        }, {
          a = 2,
        } },
      repeated_foreign_message = { {
          c = 3,
        } },
    },
  },
  {
    name = "repeated single element",
    note = "one-element packed block",
    golden = "\250\001\001\001",
    expected = {
      repeated_int32 = { 1 },
    },
  },
  {
    name = "maps with 32-bit keys",
    note = "scalar keys usable as Lua table keys",
    golden = "\194\003\004\008\001\016\002\194\003\013\008\253\255\255\255\255\255\255\255\255\001\016\004\210\003\004\008\005\016\006\226\003\004\008\013\016\016\242\003\010\013\009\000\000\000\021\010\000\000\000\130\004\010\013\011\000\000\000\021\012\000\000\000\146\004\007\008\001\021\000\000\192?\154\004\011\008\002\017\000\000\000\000\000\000\002@\162\004\004\008\001\016\000",
    expected = {
      map_int32_int32 = {
        [-3] = 4,
        [1] = 2,
      },
      map_uint32_uint32 = {
        [5] = 6,
      },
      map_sint32_sint32 = {
        [-7] = 8,
      },
      map_fixed32_fixed32 = {
        [9] = 10,
      },
      map_sfixed32_sfixed32 = {
        [11] = 12,
      },
      map_int32_float = {
        [1] = 1.5,
      },
      map_int32_double = {
        [2] = 2.25,
      },
      map_bool_bool = {
        [true] = false,
      },
    },
  },
  {
    name = "maps with 64-bit keys",
    note = "keys arrive as Int64 tables, not lookupable by value",
    golden = "\202\003\004\008\001\016\002\218\003\004\008\003\016\004\234\003\004\008\009\016\012\250\003\018\009\007\000\000\000\000\000\000\000\017\008\000\000\000\000\000\000\000\138\004\018\009\247\255\255\255\255\255\255\255\017\010\000\000\000\000\000\000\000",
    expected = {
      map_int64_int64 = {
        [I(0, 1)] = I(0, 2),
      },
      map_uint64_uint64 = {
        [I(0, 3)] = I(0, 4),
      },
      map_sint64_sint64 = {
        [I(4294967295, 4294967291)] = I(0, 6),
      },
      map_fixed64_fixed64 = {
        [I(0, 7)] = I(0, 8),
      },
      map_sfixed64_sfixed64 = {
        [I(4294967295, 4294967287)] = I(0, 10),
      },
    },
  },
  {
    name = "maps with string keys",
    note = "message and enum values",
    golden = "\170\004\012\010\005alpha\018\003one\170\004\011\010\004beta\018\003two\178\004\011\010\005gamma\018\002\000\255\186\004\011\010\005delta\018\002\008\005\194\004\013\010\007epsilon\018\002\008\006\202\004\008\010\004zeta\016\002\210\004\007\010\003eta\016\001",
    expected = {
      map_string_string = {
        ["alpha"] = "one",
        ["beta"] = "two",
      },
      map_string_bytes = {
        ["gamma"] = "\000\255",
      },
      map_string_nested_message = {
        ["delta"] = {
          a = 5,
        },
      },
      map_string_foreign_message = {
        ["epsilon"] = {
          c = 6,
        },
      },
      map_string_nested_enum = {
        ["zeta"] = 2,
      },
      map_string_foreign_enum = {
        ["eta"] = 1,
      },
    },
  },
  {
    name = "map entries holding zero values",
    note = "producer may drop either side of the entry",
    golden = "\194\003\004\008\000\016\000\170\004\004\010\000\018\000",
    expected = {
      map_int32_int32 = {
        [0] = 0,
      },
      map_string_string = {
        [""] = "",
      },
    },
  },
  {
    name = "map with negative sfixed32 key",
    note = "negative sfixed32 as a map key and value",
    golden = "\130\004\010\013\245\255\255\255\021\244\255\255\255",
    expected = {
      map_sfixed32_sfixed32 = {
        [-11] = -12,
      },
    },
  },
  {
    name = "map entry with the key omitted",
    note = "producer dropped a zero key; decode applies the type's default",
    golden = "\194\003\002\016\007",
    expected = {
      map_int32_int32 = {
        [0] = 7,
      },
    },
  },
  {
    name = "map entry with the value omitted",
    note = "producer dropped a zero value",
    golden = "\194\003\002\008\005",
    expected = {
      map_int32_int32 = {
        [5] = 0,
      },
    },
  },
  {
    name = "map entry with both sides omitted",
    note = "empty entry decodes to the zero key and zero value",
    golden = "\194\003\000",
    expected = {
      map_int32_int32 = {
        [0] = 0,
      },
    },
  },
  {
    name = "string map entry with the key omitted",
    note = "the dropped side defaults to the empty string, not nil",
    golden = "\170\004\006\018\004only",
    expected = {
      map_string_string = {
        [""] = "only",
      },
    },
  },
  {
    name = "int32 from a five-byte payload",
    note = "a uint32 producer's 0xFFFFFFFF read back through a wire-compatible int32",
    golden = "\008\255\255\255\255\015",
    expected = {
      optional_int32 = -1,
    },
  },
  {
    name = "int32 at the sign boundary",
    note = "0x80000000 is the low word's first negative value",
    golden = "\008\128\128\128\128\008",
    expected = {
      optional_int32 = -2147483648,
    },
  },
  {
    name = "int32 with a populated high word",
    note = "bits above the low word are discarded rather than widening the value",
    golden = "\008\129\128\128\128\240\255\255\255\255\001",
    expected = {
      optional_int32 = 1,
    },
  },
  {
    name = "enum from a five-byte payload",
    note = "an enum truncates to 32 bits on the same path as int32",
    golden = "\168\001\255\255\255\255\015",
    expected = {
      optional_nested_enum = -1,
    },
  },
  {
    name = "uint32 from a ten-byte payload",
    note = "truncation is unsigned here, so the same bytes are not int32's -1",
    golden = "\024\255\255\255\255\255\255\255\255\255\001",
    expected = {
      optional_uint32 = 4294967295,
    },
  },
  {
    name = "sint32 from a ten-byte payload",
    note = "the zigzag input is the truncated low word, not a rounded double",
    golden = "(\255\255\255\255\255\255\255\255\255\001",
    expected = {
      optional_sint32 = -2147483648,
    },
  },
  {
    name = "oneof scalar arm",
    note = "ordinary field on the wire",
    golden = "\248\006\011",
    expected = {
      oneof_uint32 = 11,
    },
  },
  {
    name = "oneof message arm",
    note = "ordinary length-delimited field",
    golden = "\130\007\002\008\012",
    expected = {
      oneof_nested_message = {
        a = 12,
      },
    },
  },
  {
    name = "unusual field names",
    note = "names that survive the schema round trip",
    golden = "\136\025\001\152\025\003\160\025\004\192\025\008\216\025\011\136\026\017",
    expected = {
      fieldname1 = 1,
      _field_name3 = 3,
      field__name4_ = 4,
      FieldName8 = 8,
      FIELD_NAME11 = 11,
      field_name17__ = 17,
    },
  },
  {
    name = "empty message",
    note = "no fields set at all",
    golden = "",
    expected = {},
  },
}
