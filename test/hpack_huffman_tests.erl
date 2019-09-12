
-module(hpack_huffman_tests).

-include_lib("eunit/include/eunit.hrl").


www_example_com_test() ->
    Expect = <<
        16#f1, 16#e3, 16#c2, 16#e5,
        16#f2, 16#3a, 16#6b, 16#a0,
        16#ab, 16#90, 16#f4, 16#ff
    >>,
    ?assertEqual(Expect, hpack_huffman:encode(<<"www.example.com">>)),
    ?assertEqual(<<"www.example.com">>, hpack_huffman:decode(Expect)).
