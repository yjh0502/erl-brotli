-module(brotli_encoder_SUITE).

-compile(export_all).

-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").

all() -> [fixture_equal, large_file].

fixture_equal(Config) ->
    DataDir = ?config(data_dir, Config),
    InDir = filename:join(DataDir, "in"),
    OutDir = filename:join(DataDir, "out"),
    {ok, InFiles} = file:list_dir(InDir),

    Test = fun(FileName) ->
                   InFile = filename:join(InDir, FileName),
                   OutFile = filename:join(OutDir, FileName),
                   ct:log("~s = encode(~s)~n", [OutFile, InFile]),
                   {ok, In} = file:read_file(InFile),
                   {ok, Out} = file:read_file(OutFile),
                   ?assertEqual({ok, Out}, brotli:encode(In))
           end,

    [Test(Name) || Name <- InFiles].

large_file(Config) ->
    DataDir = ?config(data_dir, Config),
    InFile = filename:join(DataDir, "large"),
    Sha256Sum = <<"24AF7F424EF5433FA9C1BD741B041E8D718162E42BE429858BA042630AB7D0F2">>,
    Encoder = brotli_encoder:new(),
    Hasher = crypto:hash_init(sha256),

    {ok, File} = file:open(InFile, [read, raw, binary]),

    Hash = compress_and_hash(File, Encoder, Hasher),

    ?assertEqual(Sha256Sum, binary:encode_hex(Hash)).

compress_and_hash(File, Encoder, Hasher) ->
    case file:read(File, 1024) of
        {ok, Data} ->
            {ok, Compressed} = brotli_encoder:append(Encoder, Data),
            NewHasher = crypto:hash_update(Hasher, Compressed),
            compress_and_hash(File, Encoder, NewHasher);
        eof ->
            {ok, Compressed} = brotli_encoder:finish(Encoder),
            NewHasher = crypto:hash_update(Hasher, Compressed),
            crypto:hash_final(NewHasher)
    end.
