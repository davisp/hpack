-module(hpack_nginx_tests).

-include_lib("eunit/include/eunit.hrl").


-define(DATA_FILE, "test/nginx.data").


nginx_test_() ->
    {ok, Stories} = file:consult(?DATA_FILE),
    lists:map(fun({Story, Cases}) ->
        {atom_to_list(Story), fun() -> run_story(Cases) end}
    end, Stories).


run_story(Cases) ->
    run_story(hpack:new(), Cases).


run_story(_Ctx, []) ->
    ok;
run_story(Ctx, [Case | Cases]) ->
    NewCtx = run_case(Ctx, Case),
    run_story(NewCtx, Cases).


run_case(Ctx0, {Size, Wire, Headers}) ->
    Ctx1 = if Size == undefined -> Ctx0; true ->
        hpack:resize(Ctx0, Size)
    end,

    Data = hpack_tutil:dehex(Wire),
    {ok, Ctx2, Decoded1} = hpack:decode(Ctx1, Data),
    ?assert(hpack_tutil:headers_equal(Headers, Decoded1)),

    {ok, _, Encoded} = hpack:encode(Ctx1, Headers),
    {ok, _, Decoded2} = hpack:decode(Ctx1, Encoded),
    ?assert(hpack_tutil:headers_equal(Headers, Decoded2)),

    Ctx2.

