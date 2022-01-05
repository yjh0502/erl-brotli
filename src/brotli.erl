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
-export([decode/1, decode/2]).

%% @doc Return version of the underlying Brotli C implementation.
%%
%% This is <b>not</b> version of the Erlang library.
%% @end
-spec version() -> {Major :: integer(), Minor :: integer(), Patch :: integer()}.
version() ->
    brotli_nif:version().

%% @doc Maximal possible size of the compressed data of size `Size'.
%% @end
-spec max_compressed_size(non_neg_integer()) -> non_neg_integer().
max_compressed_size(Size) ->
    brotli_nif:max_compressed_size(Size).

%% @doc Compress data in one step.
%% @end
-spec encode(Data :: iodata()) -> {ok, iodata()} | error.
encode(Data) ->
    encode(Data, #{}).

%% @doc Compress data in one step with custom options.
%% @end
-spec encode(Data :: iodata(), Opts :: brotli_encoder:options()) -> {ok, iodata()} | error.
encode(Data, Opts0) when is_map(Opts0) ->
    Opts = maps:put(size_hint, iolist_size(Data), Opts0),
    Encoder = brotli_encoder:new(Opts),
    case encode_chunks(Encoder, iolist_to_binary(Data), []) of
        {ok, Compressed} ->
            case brotli_encoder:is_finished(Encoder) of
                true -> {ok, iolist_to_binary(Compressed)};
                false -> {error, nf}
            end;
        Other ->
            Other
    end.

encode_chunks(Encoder, <<Data:(1024 * 1024 + 256 * 1024)/binary, Rest/binary>>, Acc) ->
    case brotli_encoder:append(Encoder, Data) of
        {ok, Compressed} ->
            encode_chunks(Encoder, Rest, [Compressed | Acc]);
        Other ->
            Other
    end;
encode_chunks(Encoder, Data, Acc) ->
    case brotli_encoder:finish(Encoder, Data) of
        {ok, Compressed} ->
            {ok, lists:reverse([Compressed | Acc])};
        Other ->
            Other
    end.

decode(Data) ->
    decode(Data, #{}).
decode(Data, _Opts) ->
    Decoder = brotli_decoder:new(),
    case brotli_decoder:stream(Decoder, Data) of
        {ok, _} = Result ->
            case brotli_decoder:is_finished(Decoder) of
                true -> Result;
                false -> error
            end;
        _ ->
            error
    end.
