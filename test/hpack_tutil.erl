
-module(hpack_tutil).


-export([
    random_int/0,
    random_int/1,

    random_bin/0,
    random_bin/1,

    dehex/1
]).


random_int() ->
    random_int(16#FFFFFFFF).


random_int(Range) ->
    rand:uniform(Range).


random_bin() ->
    random_bin(1024).


random_bin(Size) ->
    Bytes = [
        random_int(256) - 1
        || _ <- lists:seq(1, Size)
    ],
    list_to_binary(Bytes).


dehex(Binary) ->
    dehex(strip(Binary, []), []).


dehex(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));

dehex(<<Hi:8, Lo:8, Rest/binary>>, Acc) ->
    HN = nibble(Hi),
    LN = nibble(Lo),
    dehex(Rest, [<<HN:4, LN:4>> | Acc]).


strip(<<>>, Acc) ->
    list_to_binary(lists:reverse(Acc));

strip(<<" ", Rest/binary>>, Acc) ->
    strip(Rest, Acc);

strip(<<"\n", Rest/binary>>, Acc) ->
    strip(Rest, Acc);

strip(<<Byte:8, Rest/binary>>, Acc) ->
    strip(Rest, [Byte | Acc]).


nibble($0) ->  0;
nibble($1) ->  1;
nibble($2) ->  2;
nibble($3) ->  3;
nibble($4) ->  4;
nibble($5) ->  5;
nibble($6) ->  6;
nibble($7) ->  7;
nibble($8) ->  8;
nibble($9) ->  9;
nibble($a) -> 10;
nibble($A) -> 10;
nibble($b) -> 11;
nibble($B) -> 11;
nibble($c) -> 12;
nibble($C) -> 12;
nibble($d) -> 13;
nibble($D) -> 13;
nibble($e) -> 14;
nibble($E) -> 14;
nibble($f) -> 15;
nibble($F) -> 15.
