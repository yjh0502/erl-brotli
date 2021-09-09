%%% Copyright (c) 2016 Jihyun Yu <yjh0502@gmail.com>
%%% Copyright (c) 2021 Łukasz Niemier <lukasz@niemier.pl>
%%% All rights reserved.
%%%
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%%
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in
%%%    the documentation and/or other materials provided with the
%%%    distribution.
%%%
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%%% COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%%% POSSIBILITY OF SUCH DAMAGE.

-module(brotli).

-export([version/0, max_compressed_size/1]).
-export([encode/1, encode/2]).

version() ->
    brotli_nif:version().

max_compressed_size(Size) ->
    brotli_nif:max_compressed_size(Size).

encode(Data) ->
    encode(Data, #{}).

encode(Data, Opts0) when is_map(Opts0) ->
    Opts = maps:put(size_hint, byte_size(Data), Opts0),
    Encoder = brotli_encoder:new(Opts),
    brotli_encoder:finish(Encoder, Data).

-ifdef(EUNIT).

-include_lib("eunit/include/eunit.hrl").

encode_simple_test_() ->
    Compressed = <<11, 2, 128, 104, 101, 108, 108, 111, 3>>,
    [?_assertEqual({ok, Compressed}, encode(<<"hello">>)),
     ?_assertEqual({ok, Compressed}, encode(<<"hello">>, #{})),
     ?_assertEqual({ok, Compressed}, encode(<<"hello">>, #{mode => generic})),
     ?_assertEqual({ok, Compressed}, encode(<<"hello">>, #{mode => text})),
     ?_assertEqual({ok, Compressed}, encode(<<"hello">>, #{mode => font}))]
    ++ lists:map(fun(Level) ->
                         ?_assertEqual({ok, Compressed}, encode(<<"hello">>, #{quality => Level}))
                 end,
                 lists:seq(0, 11)).

-endif.
