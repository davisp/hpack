
-module(hpack_rfc5741_tests).

-include_lib("eunit/include/eunit.hrl").

% All test data is from Section C in RFC 7541
% https://httpwg.org/specs/rfc7541.html#rfc.section.C

c_1_1_test() ->
    ?assertEqual(<<10:5>>, hpack_integer:encode(10, 5)).


c_1_2_test() ->
    ?assertEqual(<<252, 208, 10:5>>, hpack_integer:encode(1337, 5)).


c_2_1_test() ->
    Ctx = hpack:new_context(),
    Headers = [
        {<<"custom-key">>, <<"custom-value">>, [uncompressed]}
    ],
    Hex = <<"
        400a 6375 7374 6f6d 2d6b 6579 0d63 7573
        746f 6d2d 6865 6164 6572
    ">>,
    Table = {55, [
        {1, <<"custom-key">>, <<"custom-value">>}
    ]},

    check_correct(Ctx, Headers, Hex, Table).


c_2_2_test() ->
    Ctx = hpack:new_context(),
    Headers = [
        {<<":path">>, <<"/sample/path">>, [uncompressed, no_index]}
    ],
    Hex = <<"
        040c 2f73 616d 706c 652f 7061 7468
    ">>,
    Table = {0, []},

    check_correct(Ctx, Headers, Hex, Table).



c_2_3_test() ->
    Ctx = hpack:new_context(),
    Headers = [
        {<<"password">>, <<"secret">>, [uncompressed, never_indexed]}
    ],
    Hex = <<"
        1008 7061 7373 776f 7264 0673 6563 7265
        74
    ">>,
    Table = {0, []},

    check_correct(Ctx, Headers, Hex, Table).


c_2_4_test() ->
    Ctx = hpack:new_context(),
    Headers = [{<<":method">>, <<"GET">>}],
    Hex = <<"
        82
    ">>,
    Table = {0, []},

    check_correct(Ctx, Headers, Hex, Table).


c_3_test() ->
    Headers1 = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"http">>},
        {<<":path">>, <<"/">>},
        {<<":authority">>, <<"www.example.com">>, [uncompressed]}
    ],
    Hex1 = <<"
        8286 8441 0f77 7777 2e65 7861 6d70 6c65
        2e63 6f6d
    ">>,
    Table1 = {57, [
        {1, <<":authority">>, <<"www.example.com">>}
    ]},

    Headers2 = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"http">>},
        {<<":path">>, <<"/">>},
        {<<":authority">>, <<"www.example.com">>, [uncompressed]},
        {<<"cache-control">>, <<"no-cache">>, [uncompressed]}
    ],
    Hex2 = <<"
        8286 84be 5808 6e6f 2d63 6163 6865
    ">>,
    Table2 = {110, [
        {1, <<"cache-control">>, <<"no-cache">>},
        {2, <<":authority">>, <<"www.example.com">>}
    ]},

    Headers3 = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"https">>},
        {<<":path">>, <<"/index.html">>},
        {<<":authority">>, <<"www.example.com">>, [uncompressed]},
        {<<"custom-key">>, <<"custom-value">>, [uncompressed]}
    ],
    Hex3 = <<"
        8287 85bf 400a 6375 7374 6f6d 2d6b 6579
        0c63 7573 746f 6d2d 7661 6c75 65
    ">>,
    Table3 = {164, [
        {1, <<"custom-key">>, <<"custom-value">>},
        {2, <<"cache-control">>, <<"no-cache">>},
        {3, <<":authority">>, <<"www.example.com">>}
    ]},

    Ctx1 = hpack:new_context(),
    Ctx2 = check_correct(Ctx1, Headers1, Hex1, Table1),
    Ctx3 = check_correct(Ctx2, Headers2, Hex2, Table2),
    check_correct(Ctx3, Headers3, Hex3, Table3).


c_4_test() ->
    Headers1 = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"http">>},
        {<<":path">>, <<"/">>},
        {<<":authority">>, <<"www.example.com">>}
    ],
    Hex1 = <<"
        8286 8441 8cf1 e3c2 e5f2 3a6b a0ab 90f4
        ff
    ">>,
    Table1 = {57, [
        {1, <<":authority">>, <<"www.example.com">>}
    ]},

    Headers2 = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"http">>},
        {<<":path">>, <<"/">>},
        {<<":authority">>, <<"www.example.com">>},
        {<<"cache-control">>, <<"no-cache">>}
    ],
    Hex2 = <<"
        8286 84be 5886 a8eb 1064 9cbf
    ">>,
    Table2 = {110, [
        {1, <<"cache-control">>, <<"no-cache">>},
        {2, <<":authority">>, <<"www.example.com">>}
    ]},

    Headers3 = [
        {<<":method">>, <<"GET">>},
        {<<":scheme">>, <<"https">>},
        {<<":path">>, <<"/index.html">>},
        {<<":authority">>, <<"www.example.com">>},
        {<<"custom-key">>, <<"custom-value">>}
    ],
    Hex3 = <<"
        8287 85bf 4088 25a8 49e9 5ba9 7d7f 8925
        a849 e95b b8e8 b4bf
    ">>,
    Table3 = {164, [
        {1, <<"custom-key">>, <<"custom-value">>},
        {2, <<"cache-control">>, <<"no-cache">>},
        {3, <<":authority">>, <<"www.example.com">>}
    ]},

    Ctx1 = hpack:new_context(),
    Ctx2 = check_correct(Ctx1, Headers1, Hex1, Table1),
    Ctx3 = check_correct(Ctx2, Headers2, Hex2, Table2),
    check_correct(Ctx3, Headers3, Hex3, Table3).


c_5_test() ->
    Headers1 = [
        {<<":status">>, <<"302">>, [uncompressed]},
        {<<"cache-control">>, <<"private">>, [uncompressed]},
        {<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>, [uncompressed]},
        {<<"location">>, <<"https://www.example.com">>, [uncompressed]}
    ],
    Hex1 = <<"
        4803 3330 3258 0770 7269 7661 7465 611d
        4d6f 6e2c 2032 3120 4f63 7420 3230 3133
        2032 303a 3133 3a32 3120 474d 546e 1768
        7474 7073 3a2f 2f77 7777 2e65 7861 6d70
        6c65 2e63 6f6d
    ">>,
    Table1 = {222, [
        {1, <<"location">>, <<"https://wwww.example.com">>},
        {2, <<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
        {3, <<"cache-control">>, <<"private">>},
        {4, <<":status">>, <<"302">>}
    ]},

    Headers2 = [
        {<<":status">>, <<"307">>, [uncompressed]},
        {<<"cache-control">>, <<"private">>, [uncompressed]},
        {<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>, [uncompressed]},
        {<<"location">>, <<"https://www.example.com">>, [uncompressed]}
    ],
    Hex2 = <<"
        4803 3330 37c1 c0bf
    ">>,
    Table2 = {222, [
        {1, <<":status">>, <<"307">>},
        {2, <<"location">>, <<"https://wwww.example.com">>},
        {3, <<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
        {4, <<"cache-control">>, <<"private">>}
    ]},

    Headers3 = [
        {<<":status">>, <<"200">>},
        {<<"cache-control">>, <<"private">>, [uncompressed]},
        {<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>, [uncompressed]},
        {<<"content-encoding">>, <<"gzip">>, [uncompressed]},
        {
            <<"set-cookie">>,
            <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>,
            [uncompressed]
        }
    ],
    Hex3 = <<"
        88c1 611d 4d6f 6e2c 2032 3120 4f63 7420
        3230 3133 2032 303a 3133 3a32 3220 474d
        54c0 5a04 677a 6970 7738 666f 6f3d 4153
        444a 4b48 514b 425a 584f 5157 454f 5049
        5541 5851 5745 4f49 553b 206d 6178 2d61
        6765 3d33 3630 303b 2076 6572 7369 6f6e
        3d31
    ">>,
    Table3 = {215, [
        {
            1,
            <<"set-cookie">>,
            <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>
        },
        {2, <<"content-encoding">>, <<"gzip">>},
        {3, <<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>}
    ]},

    Ctx1 = hpack:new_context(256),
    Ctx2 = check_correct(Ctx1, Headers1, Hex1, Table1),
    Ctx3 = check_correct(Ctx2, Headers2, Hex2, Table2),
    check_correct(Ctx3, Headers3, Hex3, Table3).


c_6_test() ->
    Headers1 = [
        {<<":status">>, <<"302">>},
        {<<"cache-control">>, <<"private">>},
        {<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
        {<<"location">>, <<"https://www.example.com">>}
    ],
    Hex1 = <<"
        4882 6402 5885 aec3 771a 4b61 96d0 7abe
        9410 54d4 44a8 2005 9504 0b81 66e0 82a6
        2d1b ff6e 919d 29ad 1718 63c7 8f0b 97c8
        e9ae 82ae 43d3
    ">>,
    Table1 = {222, [
        {1, <<"location">>, <<"https://wwww.example.com">>},
        {2, <<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
        {3, <<"cache-control">>, <<"private">>},
        {4, <<":status">>, <<"302">>}
    ]},

    Headers2 = [
        {<<":status">>, <<"307">>},
        {<<"cache-control">>, <<"private">>},
        {<<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
        {<<"location">>, <<"https://www.example.com">>}
    ],
    Hex2 = <<"
        4883 640e ffc1 c0bf
    ">>,
    Table2 = {222, [
        {1, <<":status">>, <<"307">>},
        {2, <<"location">>, <<"https://wwww.example.com">>},
        {3, <<"date">>, <<"Mon, 21 Oct 2013 20:13:21 GMT">>},
        {4, <<"cache-control">>, <<"private">>}
    ]},

    Headers3 = [
        {<<":status">>, <<"200">>},
        {<<"cache-control">>, <<"private">>},
        {<<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>},
        {<<"content-encoding">>, <<"gzip">>},
        {
            <<"set-cookie">>,
            <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>
        }
    ],
    Hex3 = <<"
        88c1 6196 d07a be94 1054 d444 a820 0595
        040b 8166 e084 a62d 1bff c05a 839b d9ab
        77ad 94e7 821d d7f2 e6c7 b335 dfdf cd5b
        3960 d5af 2708 7f36 72c1 ab27 0fb5 291f
        9587 3160 65c0 03ed 4ee5 b106 3d50 07
    ">>,
    Table3 = {215, [
        {
            1,
            <<"set-cookie">>,
            <<"foo=ASDJKHQKBZXOQWEOPIUAXQWEOIU; max-age=3600; version=1">>
        },
        {2, <<"content-encoding">>, <<"gzip">>},
        {3, <<"date">>, <<"Mon, 21 Oct 2013 20:13:22 GMT">>}
    ]},

    Ctx1 = hpack:new_context(256),
    Ctx2 = check_correct(Ctx1, Headers1, Hex1, Table1),
    Ctx3 = check_correct(Ctx2, Headers2, Hex2, Table2),
    check_correct(Ctx3, Headers3, Hex3, Table3).



check_correct(Ctx, Headers, HexEncoded, Table) ->
    Encoded = hpack_tutil:dehex(HexEncoded),
    {ok, EncCtx, EncodeResult} = hpack:encode(Ctx, Headers),
    {ok, DecCtx, DecodeResult} = hpack:decode(Ctx, Encoded),

    ?assertEqual(Encoded, EncodeResult),
    ?assertEqual(Headers, DecodeResult),
    ?assertEqual(Table, hpack_index:table(EncCtx)),
    ?assertEqual(EncCtx, DecCtx),

    {ok, EncCtx}.