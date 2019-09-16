-module(hpack_tests).

-include_lib("eunit/include/eunit.hrl").


basic_nghttp2_request_test() ->
    Encoded = <<
        130, 132, 134,  65, 138, 160, 228,  29,
         19, 157,   9, 184, 240,  30,   7,  83,
          3,  42,  47,  42, 144, 122, 138, 170,
        105, 210, 154, 196, 192,  23, 117, 119,
        127
    >>,

    Decoded = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"http">>},
        {<<":authority">>, <<"localhost:8080">>},
        {<<"accept">>, <<"*/*">>, [uncompressed]},
        {<<"accept-encoding">>, <<"gzip, deflate">>},
        {<<"user-agent">>, <<"nghttp2/0.7.7">>}
    ],

    {ok, _NewCtx1, DecResult} = hpack:decode(hpack:new_context(), Encoded),
    ?assert(hpack_tutil:headers_equal(Decoded, DecResult)),

    {ok, _NewCtx2, EncResult} = hpack:encode(hpack:new_context(), Decoded),
    ?assertEqual(Encoded, EncResult).


decode_1_test() ->
    Encoded = <<
        130, 132, 134,  65, 138, 160, 228,  29,
         19, 157,   9, 184, 240,  30,  15,  83,
          3,  42,  47,  42, 144, 122, 138, 170,
        105, 210, 154, 196, 192,  23, 117, 112,
        135,  64, 135, 242, 178, 125, 117,  73,
        236, 175,   1,  66, 126,   1,  79,  64,
        133, 242, 181,  37,  63, 143,   1, 112,
        126,   1, 116, 127,   1,   1, 116
    >>,

    Decoded = [
        {<<":method">>, <<"GET">>},
        {<<":path">>, <<"/">>},
        {<<":scheme">>, <<"http">>},
        {<<":authority">>, <<"localhost:8081">>},
        {<<"accept">>, <<"*/*">>},
        {<<"accept-encoding">>, <<"gzip, deflate">>},
        {<<"user-agent">>, <<"nghttp2/0.7.11">>},
        {<<"x-tyktorp">>, <<"B">>},
        {<<"x-tyktorp">>, <<"O">>},
        {<<"x-meow">>, <<"p">>},
        {<<"x-meow">>, <<"t">>},
        {<<"x-tyktorp">>, <<"t">>}
    ],

    {ok, _NewCtx1, DecResult} = hpack:decode(hpack:new_context(), Encoded),
    ?assert(hpack_tutil:headers_equal(Decoded, DecResult)),

    {ok, _NewCtx2, EncResult} = hpack:encode(hpack:new_context(), Decoded),
    ?assertEqual(Encoded, EncResult).


decode_indexed_static_test() ->
    Bin = <<2#10001000>>,
    {ok, _Ctx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{<<":status">>, <<"200">>}], Result).


decode_literal_incremental_indexing_indexed_test() ->
    Bin = <<2#01000100, 2#00000101, "/test">>,
    {ok, _Ctx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{<<":path">>, <<"/test">>, [uncompressed]}], Result).


decode_literal_incremental_indexing_new_test() ->
    Bin = <<2#01000000, 2#00001010, "custom-key", 2#00001100, "custom-value">>,
    {ok, _Ctx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{
            <<"custom-key">>,
            <<"custom-value">>,
            [uncompressed]
        }],
        Result
    ).


decode_literal_without_indexing_indexed_test() ->
    Bin = <<2#00001111, 2#00101011, 2#00000111, "Firefox">>,
    {ok, NewCtx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{
            <<"user-agent">>,
            <<"Firefox">>,
            [uncompressed, no_index]
        }],
        Result
    ),
    ?assertEqual(hpack:new_context(), NewCtx).


decode_literal_without_indexing_new_test() ->
    Bin = <<2#00000000, 2#00001010, "custom-key", 2#00001100, "custom-value">>,
    {ok, NewCtx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{
            <<"custom-key">>,
            <<"custom-value">>,
            [uncompressed, no_index, no_name_index]
        }],
        Result
    ),
    ?assertEqual(hpack:new_context(), NewCtx).


decode_literal_never_indexed_indexed_test() ->
    Bin = <<2#00011111, 2#00101011, 2#00000111, "Firefox">>,
    {ok, NewCtx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{
            <<"user-agent">>,
            <<"Firefox">>,
            [uncompressed, never_index]
        }],
        Result
    ),
    ?assertEqual(hpack:new_context(), NewCtx).


decode_literal_never_indexed_new_test() ->
    Bin = <<2#00010000, 2#00001010, "custom-key", 2#00001100, "custom-value">>,
    {ok, NewCtx, Result} = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual([{
            <<"custom-key">>,
            <<"custom-value">>,
            [uncompressed, never_index, no_name_index]
        }],
        Result
    ),
    ?assertEqual(hpack:new_context(), NewCtx).


compression_error_on_too_large_size_increase_test() ->
    Bin = <<2#001:3,255,16,31:5>>,
    Error = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual({error, {invalid_table_size, 4097}}, Error).


compression_error_on_size_adjustment_after_headers_test() ->
    Bin = <<2#10001000, 2#001:3,255,16,0:5>>,
    Error = hpack:decode(hpack:new_context(), Bin),
    ?assertEqual({error, {invalid_size_update, headers_received}}, Error).


no_compression_error_on_small_enough_adjustment_test() ->
    Bin = <<2#001:3,255,16,0:5>>,
    ?assertMatch({ok, _NewCtx, []}, hpack:decode(hpack:new_context(), Bin)).


no_compression_error_on_two_adjustments_test() ->
    Bin = <<16#20, 16#3f, 16#e1, 16#1f>>,
    ?assertMatch({ok, _NewCtx, []}, hpack:decode(hpack:new_context(), Bin)).


compression_error_on_indexed_field_no_value_test() ->
    Result = hpack:decode(hpack:new_context(), <<16#40>>),
    ?assertEqual({error, {invalid_string, no_data}}, Result).
