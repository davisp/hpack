%% @doc
%% The `hpack' module provides functions for working with HPACK as described in
%% <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>.
%%
%% @reference <a href="https://tools.ietf.org/html/rfc7541">RFC 7541</a>

-module(hpack).


-export([
    new_context/0,
    new_context/1,

    resize/2,

    decode/2,
    encode/2,

    explain/2
]).


-include("hpack.hrl").


-export_type([
    context/0,

    header_name/0,
    header_value/0,
    header_opt/0,
    header/0,
    headers/0,

    encode_error/0,
    decode_error/0
]).


%% @equiv new_context(4096)
-spec new_context() -> context().
new_context() ->
    #hpack_ctx{}.


%% @doc
%% Returns a new HPACK context with the given `MaxTableSize' as the max table size.
-spec new_context(non_neg_integer()) -> context().
new_context(MaxTableSize) ->
    #hpack_ctx{
        max_size = min(MaxTableSize, 4096),
        conn_max_size = MaxTableSize
    }.


%% @doc
%% Updates the max table size of the given HPACK context (`Context') to the given
%% `NewSize'.
%%
%% Useful when HTTP/2 settings are renegotiated.
-spec resize(context(), non_neg_integer()) -> context().
resize(#hpack_ctx{} = Ctx, NewSize) when is_integer(NewSize) ->
    #hpack_ctx{
        max_size = OldSize,
        conn_max_size = ConnMaxSize
    } = Ctx,
    if NewSize =< ConnMaxSize -> ok; true ->
        ?ERROR({invalid_table_size, NewSize})
    end,
    case NewSize > OldSize of
        true -> Ctx#hpack_ctx{max_size = NewSize};
        false -> hpack_index:resize(Ctx, NewSize)
    end.


%% @doc
%% Encodes the given `Headers' using the given `Context'.
%%
%% When successful, returns a `{ok, {EncodedHeaders, NewContext}}' tuple where
%% `EncodedHeaders' is a binary representing the encoded headers and `NewContext'
%% is the new HPACK context.
%%
%% For example:
%% ```
%% Headers = [{<<":method">>, <<"GET">>}],
%% {ok, {EncodedHeaders, NewContext}} = hpack:encode(Headers, hpack:new_context()).
%% '''
-spec encode(context(), headers()) ->
        {ok, context(), binary()} |
        {error, encode_error()}.
encode(Ctx, Headers) ->
    try
        encode(Ctx, Headers, [])
    catch throw:{hpack_error, Error} ->
        {error, Error}
    end.


%% @doc
%% Decodes the given binary into a list of headers using the given HPACK
%% context.
%%
%% If successful, returns a `{ok, {Headers, NewContext}}' tuple where `Headers'
%% are the decoded headers and `NewContext' is the new HPACK context.
%%
%% For example:
%% ```
%% {Headers, NewContext} = hpack:decode(Binary, OldContext).
%% '''
-spec decode(context(), binary()) ->
        {ok, {headers(), context()}} |
        {error, decode_error()}.
decode(Ctx, Bin) ->
    try
        decode(Ctx, Bin, [])
    catch throw:{hpack_error, Error} ->
        {error, Error}
    end.


%% @doc
%% Explain the decoding of a given HPack binary representation
%%
%% If successful, returns `[{bitstring(), any()}]' listing the
%% steps decoding takes.
%%
%% For example:
%% ```
%% {Headers, NewContext} = hpack:decode(Binary, OldContext).
%% '''
-spec explain(context(), binary()) -> [{bitstring(), any()}].
explain(Ctx, Bin) ->
    try
        explain(Ctx, Bin, [])
    catch throw:{hpack_error, Error} ->
        Error
    end.


encode(Ctx, [], Acc) ->
    {ok, Ctx, iolist_to_binary(lists:reverse(Acc))};

encode(Ctx, [{Name, Value} | Tail], Acc) ->
    encode(Ctx, [{Name, Value, []} | Tail], Acc);

encode(Ctx, [{Name, Value, Opts} | Tail], Acc) ->
    NeverIndex = lists:member(never_index, Opts),
    NoIndex = lists:member(no_index, Opts),
    NoNameIndex = lists:member(no_name_index, Opts),

    {NewCtx, Encoded} = if
        NeverIndex and NoNameIndex ->
            {Ctx, encode_never_index(Name, Value, Opts)};
        NeverIndex ->
            {Ctx, encode_never_index(name_index(Ctx, Name), Value, Opts)};
        NoIndex and NoNameIndex ->
            {Ctx, encode_no_index(Name, Value, Opts)};
        NoIndex ->
            {Ctx, encode_no_index(name_index(Ctx, Name), Value, Opts)};
        true ->
            case hpack_index:match(Ctx, {Name, Value}) of
                {hdr_indexed, Idx} ->
                    {Ctx, encode_indexed(Idx)};
                {name_indexed, Idx} ->
                    {
                        hpack_index:add(Ctx, Name, Value),
                        encode_indexed(Idx, Value, Opts)
                    };
                not_indexed ->
                    {
                        hpack_index:add(Ctx, Name, Value),
                        encode_indexed(Name, Value, Opts)
                    }
            end
    end,

    encode(NewCtx, Tail, [Encoded | Acc]).


encode_never_index(Idx, Value, Opts) when is_integer(Idx) ->
    Prefix = <<2#0001:4>>,
    IdxBin = hpack_integer:encode(Idx, 4),
    ValueBin = hpack_string:encode(Value, Opts),
    list_to_bitstring([Prefix, IdxBin, ValueBin]);

encode_never_index(Name, Value, Opts) when is_binary(Name) ->
    Prefix = <<2#0001:4, 0:4>>,
    NameBin = hpack_string:encode(Name, Opts),
    ValueBin = hpack_string:encode(Value, Opts),
    list_to_bitstring([Prefix, NameBin, ValueBin]).


encode_no_index(Idx, Value, Opts) when is_integer(Idx) ->
    Prefix = <<2#0000:4>>,
    IdxBin = hpack_integer:encode(Idx, 4),
    ValueBin = hpack_string:encode(Value, Opts),
    list_to_bitstring([Prefix, IdxBin, ValueBin]);

encode_no_index(Name, Value, Opts) when is_binary(Name) ->
    Prefix = <<2#0000:4, 0:4>>,
    NameBin = hpack_string:encode(Name, Opts),
    ValueBin = hpack_string:encode(Value, Opts),
    list_to_bitstring([Prefix, NameBin, ValueBin]).


encode_indexed(Idx) when Idx < 63 ->
    <<2#1:1, Idx:7>>;

encode_indexed(Idx) ->
    Encoded = hpack_integer:encode(Idx, 7),
    <<2#1:1, Encoded/bits>>.


encode_indexed(Idx, Value, Opts) when is_integer(Idx) ->
    Prefix = <<2#01:2>>,
    IdxBin = hpack_integer:encode(Idx, 6),
    ValueBin = hpack_string:encode(Value, Opts),
    list_to_bitstring([Prefix, IdxBin, ValueBin]);

encode_indexed(Name, Value, Opts) when is_binary(Name) ->
    Prefix = <<2#01:2, 2#000000:6>>,
    NameBin = hpack_string:encode(Name, Opts),
    ValueBin = hpack_string:encode(Value, Opts),
    list_to_bitstring([Prefix, NameBin, ValueBin]).


name_index(Ctx, Name) when is_binary(Name) ->
    case hpack_index:match(Ctx, {Name, undefined}) of
        {_, Idx} when is_integer(Idx) -> Idx;
        not_indexed -> Name
    end.


decode(Ctx, <<>>, Acc) ->
    {ok, Ctx, lists:reverse(Acc)};

decode(Ctx, <<2#1:1, _/bits>> = Bin, Acc) ->
    decode_indexed(Ctx, Bin, Acc);

decode(Ctx, <<2#01:2, _/bits>> = Bin, Acc) ->
    decode_and_index(Ctx, Bin, Acc);

decode(Ctx, <<2#0000:4, _/bits>> = Bin, Acc) ->
    decode_no_index(Ctx, Bin, Acc);

decode(Ctx, <<2#0001:4, _/bits>> = Bin, Acc) ->
    decode_never_index(Ctx, Bin, Acc);

decode(Ctx, <<2#001:3, _/bits>> = Bin, Acc) ->
    decode_size_update(Ctx, Bin, Acc);

decode(_Ctx, Bin, _Acc) ->
    ?ERROR({invalid_packet, Bin}).


decode_indexed(_Ctx, <<2#1:1, 2#0000000:1, _/binary>>, _Acc) ->
    ?ERROR({invalid_index, 0});

decode_indexed(Ctx, <<2#1:1, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 7),
    Header = case hpack_index:lookup(Ctx, Idx) of
        undefined -> ?ERROR({unknown_index, Idx});
        Else -> Else
    end,
    decode(Ctx, B2, [Header | Acc]).


decode_and_index(Ctx, <<2#01:2, 2#000000:6, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:any_uncompressed([B1, B2]) of
        true -> {Name, Value, [uncompressed]};
        false -> {Name, Value}
    end,
    decode(hpack_index:add(Ctx, Name, Value), B3, [Header | Acc]);

decode_and_index(Ctx, <<2#01:2, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 6),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined -> ?ERROR({unknown_index, Idx});
        Else -> Else
    end,
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:is_uncompressed(B2) of
        true -> {Name, Value, [uncompressed]};
        false -> {Name, Value}
    end,
    decode(hpack_index:add(Ctx, Name, Value), B3, [Header | Acc]);

decode_and_index(_Ctx, Bin, _Acc) ->
    ?ERROR({invalid_packet, Bin}).


decode_no_index(Ctx, <<2#0000:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:any_uncompressed([B1, B2]) of
        true -> {Name, Value, [uncompressed, no_index, no_name_index]};
        false -> {Name, Value, [no_index, no_name_index]}
    end,
    decode(Ctx, B3, [Header | Acc]);

decode_no_index(Ctx, <<2#0000:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined -> ?ERROR({invalid_index, Idx});
        Else -> Else
    end,
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:is_uncompressed(B2) of
        true -> {Name, Value, [uncompressed, no_index]};
        false -> {Name, Value, [no_index]}
    end,
    decode(Ctx, B3, [Header | Acc]);

decode_no_index(_Ctx, Bin, _Acc) ->
    ?ERROR({invalid_packet, Bin}).


decode_never_index(Ctx, <<2#0001:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:decode(B1),
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:any_uncompressed([B1, B2]) of
        true -> {Name, Value, [uncompressed, never_index, no_name_index]};
        false -> {Name, Value, [never_index, no_name_index]}
    end,
    decode(Ctx, B3, [Header | Acc]);

decode_never_index(Ctx, <<2#0001:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined -> ?ERROR({invalid_index, Idx});
        Else -> Else
    end,
    {Value, B3} = hpack_string:decode(B2),
    Header = case hpack_string:is_uncompressed(B2) of
        true -> {Name, Value, [uncompressed, never_index]};
        false -> {Name, Value, [never_index]}
    end,
    decode(Ctx, B3, [Header | Acc]);

decode_never_index(_Ctx, Bin, _Acc) ->
    ?ERROR({invalid_packet, Bin}).


% TODO: Test whether resize has to precede all headers
decode_size_update(Ctx, <<2#001:3, B1/bits>>, []) ->
    {NewSize, B2} = hpack_integer:decode(B1, 5),
    decode(resize(Ctx, NewSize), B2, []);

decode_size_update(_Ctx, Bin, _Acc) ->
    ?ERROR({invalid_packet, Bin}).



explain(Ctx, <<>>, Acc) ->
    {ok, Ctx, lists:reverse(Acc)};

explain(Ctx, <<2#1:1, _/bits>> = Bin, Acc) ->
    explain_indexed(Ctx, Bin, [{<<2#1:1>>, indexed_header} | Acc]);

explain(Ctx, <<2#01:2, _/bits>> = Bin, Acc) ->
    explain_and_index(Ctx, Bin, [{<<2#01:2>>, incremental_index} | Acc]);

explain(Ctx, <<2#0000:4, _/bits>> = Bin, Acc) ->
    explain_no_index(Ctx, Bin, [{<<2#0000:4>>, no_index} | Acc]);

explain(Ctx, <<2#0001:4, _/bits>> = Bin, Acc) ->
    explain_never_index(Ctx, Bin, [{<<2#0001:4>>, never_index} | Acc]);

explain(Ctx, <<2#001:3, _/bits>> = Bin, Acc) ->
    explain_size_update(Ctx, Bin, [{<<2#001:3>>, size_update} | Acc]);

explain(_Ctx, Bin, Acc) ->
    ?ERROR(lists:reverse(Acc, [{invalid_packet, Bin}])).


explain_indexed(_Ctx, <<2#1:1, 2#0000000:1, _/binary>>, Acc) ->
    ?ERROR(lists:reverse(Acc, [{invalid_index, 0}]));

explain_indexed(Ctx, <<2#1:1, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 7),
    Header = case hpack_index:lookup(Ctx, Idx) of
        undefined ->
            ?ERROR(lists:reverse(Acc, [{unknown_index, Idx}]));
        Else ->
            Else
    end,
    explain(Ctx, B2, [Header | Acc]).


explain_and_index(Ctx, <<2#01:2, 2#000000:6, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:explain(B1),
    {Value, B3} = hpack_string:explain(B2),

    NameBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, {string, Value}},
        {NameBits, {string, Name}},
        {<<2#000000:6>>, unindexed_name}
    ] ++ Acc,

    explain(hpack_index:add(Ctx, Name, Value), B3, NewAcc);

explain_and_index(Ctx, <<2#01:2, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 6),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined ->
            ?ERROR(lists:reverse(Acc, [{unknown_index, Idx}]));
        Else ->
            Else
    end,
    {Value, B3} = hpack_string:explain(B2),

    IdxBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, Value},
        {IdxBits, {indexed_name, Idx, Name}}
    ] ++ Acc,

    explain(hpack_index:add(Ctx, Name, Value), B3, NewAcc);

explain_and_index(_Ctx, Bin, Acc) ->
    ?ERROR(lists:reverse(Acc, [{invalid_packet, Bin}])).


explain_no_index(Ctx, <<2#0000:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:explain(B1),
    {Value, B3} = hpack_string:explain(B2),

    NameBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, Value},
        {NameBits, Name},
        {<<2#0000:4>>, unindexed_name}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc);

explain_no_index(Ctx, <<2#0000:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined ->
            ?ERROR(lists:reverse(Acc, [{invalid_index, Idx}]));
        Else ->
            Else
    end,
    {Value, B3} = hpack_string:explain(B2),

    IdxBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, Value},
        {IdxBits, {indexed_name, Idx, Name}}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc);

explain_no_index(_Ctx, Bin, Acc) ->
    ?ERROR(lists:reverse(Acc, [{invalid_packet, Bin}])).


explain_never_index(Ctx, <<2#0001:4, 2#0000:4, B1/bits>>, Acc) ->
    {Name, B2} = hpack_string:explain(B1),
    {Value, B3} = hpack_string:explain(B2),

    NameBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, Value},
        {NameBits, Name},
        {<<2#0000:4>>, unindexed_name}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc);

explain_never_index(Ctx, <<2#0001:4, B1/bits>>, Acc) ->
    {Idx, B2} = hpack_integer:decode(B1, 4),
    {Name, _} = case hpack_index:lookup(Ctx, Idx) of
        undefined -> ?ERROR({invalid_index, Idx});
        Else -> Else
    end,
    {Value, B3} = hpack_string:explain(B2),

    IdxBits = hdbinary(B1, B2),
    ValueBits = hdbinary(B2, B3),

    NewAcc = [
        {ValueBits, Value},
        {IdxBits, {indexed_name, Idx, Name}}
    ] ++ Acc,

    explain(Ctx, B3, NewAcc);

explain_never_index(_Ctx, Bin, Acc) ->
    ?ERROR(lists:reverse(Acc, [{invalid_packet, Bin}])).


% TODO: Test whether resize has to precede all headers
explain_size_update(Ctx, <<2#001:3, B1/bits>>, Acc) ->
    {NewSize, B2} = hpack_integer:decode(B1, 5),
    UpdateBits = hdbinary(B1, B2),
    NewAcc = [{UpdateBits, {new_size, NewSize}} | Acc],
    explain(resize(Ctx, NewSize), B2, NewAcc);

explain_size_update(_Ctx, Bin, Acc) ->
    ?ERROR(lists:reverse(Acc, [{invalid_packet, Bin}])).


hdbinary(B1, B2) when bit_size(B1) > bit_size(B2) ->
    Len = bit_size(B1) - bit_size(B2),
    <<H:Len/bits, _/bits>> = B1,
    H.
