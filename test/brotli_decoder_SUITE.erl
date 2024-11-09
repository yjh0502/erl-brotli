-module(brotli_decoder_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [pull_multiple].

pull_multiple(Config) ->
    DataDir = ?config(data_dir, Config),
    InFile = filename:join(DataDir, "calendar.br"),
    {ok, In} = file:read_file(InFile),
    Sha256Sum = <<"42A53DA0F6FA4D69689B8096879EC4CF0D198E03ADBB227CE630830D634AD8A7">>,

    {ok, Out} = brotli:decode(In),
    ?assertEqual(1083692, byte_size(Out)),

    Hasher0 = crypto:hash_init(sha256),
    Hasher1 = crypto:hash_update(Hasher0, Out),
    Hash = crypto:hash_final(Hasher1),

    ?assertEqual(Sha256Sum, to_hex(Hash)).

to_hex(Bin) ->
    <<<<Y>> || <<X:4>> <= Bin, Y <- integer_to_list(X, 16)>>.
