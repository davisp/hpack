%% @private

-module(hpack_string).

-export([
    encode/2,
    decode/1,

    is_uncompressed/1,
    any_uncompressed/1
]).


-include("hpack.hrl").


-spec encode(binary(), [header_opt()]) -> binary().
encode(Bin, Opts) ->
    Uncompressed = lists:member(uncompressed, Opts),
    DataBin = case Uncompressed of
        true -> Bin;
        false -> hpack_huffman:encode(Bin)
    end,
    SizeBin = hpack_integer:encode(size(DataBin), 7),
    case Uncompressed of
        true -> <<0:1, SizeBin/bits, DataBin/binary>>;
        false -> <<1:1, SizeBin/bits, DataBin/binary>>
    end.


-spec decode(binary()) -> {binary(), binary()}.
decode(<<>>) ->
    ?ERROR({invalid_string, no_data});

decode(<<Huff:1, B1/bits>>) ->
    {Length, B2} = hpack_integer:decode(B1, 7),
    <<Data:Length/binary, B3/bits>> = B2,
    Value = case Huff of
        0 -> Data;
        1 -> hpack_huffman:decode(Data)
    end,
    {Value, B3}.


is_uncompressed(<<0:1, _/bits>>) -> true;
is_uncompressed(<<1:1, _/bits>>) -> false.


any_uncompressed(Bins) when is_list(Bins) ->
    lists:any(fun is_uncompressed/1, Bins).
