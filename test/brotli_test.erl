-module(brotli_test).

-include_lib("eunit/include/eunit.hrl").

encode_simple_test_() ->
    Input = <<"hello">>,
    Compressed = <<11, 2, 128, 104, 101, 108, 108, 111, 3>>,
    [
        ?_assertEqual({ok, Compressed}, brotli:encode(Input)),
        ?_assertEqual({ok, Compressed}, brotli:encode(Input, #{})),
        ?_assertEqual({ok, Compressed}, brotli:encode(Input, #{mode => generic})),
        ?_assertEqual({ok, Compressed}, brotli:encode(Input, #{mode => text})),
        ?_assertEqual({ok, Compressed}, brotli:encode(Input, #{mode => font}))
    ] ++
        lists:map(
            fun(Level) ->
                ?_assertEqual({ok, Compressed}, brotli:encode(<<"hello">>, #{quality => Level}))
            end,
            lists:seq(0, 11)
        ).

encode_window_test_() ->
    Input = <<"hello">>,
    Fun = fun(Window) ->
        {ok, Encoded} = brotli:encode(Input, #{window => Window}),
        {ok, Decoded} = brotli:decode(Encoded),
        ?_assertEqual(Input, Decoded)
    end,
    lists:map(Fun, lists:seq(10, 24)).

encode_string_test() ->
    ?assertEqual(brotli:encode(<<"hello">>), brotli:encode("hello")).

encode_iodata_test() ->
    ?assertEqual(brotli:encode(<<"hello">>), brotli:encode(["he", $l, <<"lo">>])).

decode_simple_test() ->
    Input = <<"hello">>,
    Compressed = <<11, 2, 128, 104, 101, 108, 108, 111, 3>>,
    ?assertEqual({ok, Input}, brotli:decode(Compressed)).

version_is_3ary_tuple_test() ->
    {Major, Minor, Patch} = brotli:version(),
    ?assert(is_integer(Major)),
    ?assert(is_integer(Minor)),
    ?assert(is_integer(Patch)).
