%% @private


-module(hpack_huffman).

-export([
    encode/1,
    decode/1
]).


-include("hpack.hrl").
-include("hpack_huffman.hrl").


encode(Bin) ->
    try
        encode(Bin, [], 0, size(Bin) * 8)
    catch throw:compressed_larger ->
        {error, compressed_larger}
    end.


decode(Bin) ->
    decode(Bin, ?HUFFMAN_TREE, ?HUFFMAN_TREE, false, []).


encode(<<>>, Acc, Size, OrigSize) ->
    Remainder = Size rem 8,
    Tail = case 8 - Remainder of
        1 -> [<<  1:1>>];
        2 -> [<<  3:2>>];
        3 -> [<<  7:3>>];
        4 -> [<< 15:4>>];
        5 -> [<< 31:5>>];
        6 -> [<< 63:6>>];
        7 -> [<<127:7>>];
        8 -> []
    end,
    Extra = if Remainder == 0 -> 0; true -> 8 - Remainder end,
    if Size + Extra < OrigSize -> ok; true ->
        throw(compressed_larger)
    end,
    list_to_bitstring(lists:reverse(Acc, Tail));

encode(<<Byte:8, Rest/binary>>, Acc, Size, OrigSize) ->
    Code = element(Byte + 1, ?HUFFMAN_CODES),
    if Size + bit_size(Code) < OrigSize -> ok; true ->
        io:format(standard_error, "b: ~p vs ~p~n", [Size + bit_size(Code), OrigSize]),
        throw(compressed_larger)
    end,
    encode(Rest, [Code | Acc], Size + bit_size(Code), OrigSize).


decode(<<>>, _, _, ZeroSeen, Acc) ->
    if not ZeroSeen -> ok; true ->
        ?ERROR({invalid_huffman_encoding, partial_code})
    end,
    list_to_binary(lists:reverse(Acc));

decode(<<0:1, Rest/bits>>, {{v, Value}, _}, Tree, _ZeroSeen, Acc) ->
    decode(Rest, Tree, Tree, false, [Value | Acc]);

decode(<<0:1, Rest/bits>>, {Left, _}, Tree, _ZeroSeen, Acc) ->
    decode(Rest, Left, Tree, true, Acc);

decode(<<1:1, Rest/bits>>, {_, {v, Value}}, Tree, _ZeroSeen, Acc) ->
    if Value < 256 -> ok; true ->
        ?ERROR({invalid_huffman_encoding, internal_eos})
    end,
    decode(Rest, Tree, Tree, false, [Value | Acc]);

decode(<<1:1, Rest/bits>>, {_, Right}, Tree, ZeroSeen, Acc) ->
    decode(Rest, Right, Tree, ZeroSeen, Acc).
