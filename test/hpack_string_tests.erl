
-module(hpack_string_tests).

-include_lib("eunit/include/eunit.hrl").


basic_decode_test() ->
    Bin = <<0:1, 8:7, $C,$o,$m,$m,$a,$n,$d,$s>>,
    ?assertEqual({<<"Commands">>, <<>>}, hpack_string:decode(Bin)).


basic_decode_with_huffman_test() ->
    Bin = <<
        1:1,
        6:7, %% now it's 6 bytes instead of 8. Savings!
        2#1011110:7, %% $C  7
        2#00111:5,   %% $o  5
        2#101001:6,  %% $m  6
        2#101001:6,  %% $m  6
        2#00011:5,   %% $a  5
        2#101010:6,  %% $n  6
        2#100100:6,  %% $d  6
        2#01000:5,   %% $s +5
        2#11:2       %%   =46
    >>,
    ?assertEqual({<<"Commands">>, <<>>}, hpack_string:decode(Bin)).


roundtrip_sequential_test() ->
    lists:foreach(fun(IntValue) ->
        roundtrip(integer_to_binary(IntValue), []),
        roundtrip(integer_to_binary(IntValue), [uncompressed])
    end, lists:seq(0, 1024)).


roundtrip_random_test() ->
    lists:foreach(fun(_) ->
        Value = hpack_tutil:random_bin(256),
        roundtrip(Value, []),
        roundtrip(Value, [uncompressed])
    end, lists:seq(0, 1024)).


roundtrip(Value, Opts) ->
    Encoded = hpack_string:encode(Value, Opts),
    Decoded = hpack_string:decode(Encoded),
    ?assertEqual({Value, <<>>}, Decoded),

    Garbage = hpack_tutil:random_bin(64),
    WithGarbage = <<Encoded/binary, Garbage/binary>>,
    ?assertEqual({Value, Garbage}, hpack_string:decode(WithGarbage)).
