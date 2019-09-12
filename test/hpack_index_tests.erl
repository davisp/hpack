-module(hpack_index_tests).

-include_lib("eunit/include/eunit.hrl").


resize_test() ->
    Ctx0 = hpack:new_context(64),

    Ctx1 = hpack_index:add(Ctx0, <<"four">>, <<"four">>),
    ?assertEqual(40, hpack_index:size(Ctx1)),

    Ctx2 = hpack_index:add(Ctx1, <<"--eight-">>, <<"--eight-">>),
    ?assertEqual(48, hpack_index:size(Ctx2)).


increase_max_size_test() ->
    Ctx0 = hpack:new_context(64),

    Ctx1 = hpack_index:add(Ctx0, <<"keyA">>, <<"valA">>),
    ?assertEqual(40, hpack_index:size(Ctx1)),

    Ctx2 = hpack_index:resize(Ctx1, 128),
    ?assertEqual(40, hpack_index:size(Ctx2)).


decrease_max_size_test() ->
    Ctx0 = hpack:new_context(64),

    Ctx1 = hpack_index:add(Ctx0, <<"keyA">>, <<"valA">>),
    ?assertEqual(40, hpack_index:size(Ctx1)),

    Ctx2 = hpack_index:resize(Ctx1, 32),
    ?assertEqual(0, hpack_index:size(Ctx2)).
