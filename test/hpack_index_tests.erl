-module(hpack_index_tests).


-include("../src/hpack_index.hrl").
-include_lib("eunit/include/eunit.hrl").


size_test() ->
    Ctx0 = hpack:new_context(),
    ?assertEqual(0, hpack_index:size(Ctx0)),
    ?assertEqual(4096, hpack_index:max_size(Ctx0)).


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


index_not_found_test() ->
    Ctx = hpack:new_context(),
    ?assertEqual(undefined, hpack_index:lookup(Ctx, 70)).


match_static_exact_test() ->
    Entries = tuple_to_list(?STATIC_TABLE),
    Ctx = hpack:new_context(),
    lists:foldl(fun(Entry, Idx) ->
        ?assertEqual({hdr_indexed, Idx}, hpack_index:match(Ctx, Entry)),
        Idx + 1
    end, 1, Entries).


match_static_name_test() ->
    AllEntries = tuple_to_list(?STATIC_TABLE),
    {_, Entries} = lists:foldl(fun({Name, _}, {Idx, EntryAcc}) ->
        case lists:keyfind(Name, 1, EntryAcc) of
            {_, _} -> {Idx + 1, EntryAcc};
            false -> {Idx + 1, [{Name, Idx} | EntryAcc]}
        end
    end, {1, []}, AllEntries),
    Ctx = hpack:new_context(),
    lists:foreach(fun({HdrName, Idx}) ->
        Entry = {HdrName, undefined},
        ?assertEqual({name_indexed, Idx}, hpack_index:match(Ctx, Entry))
    end, Entries).