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

-module(brotli_encoder).

-export([new/0, new/1, set_opts/2, append/2, finish/2, finish/1]).

-export_type([t/0, mode/0, options/0, ndirect/0]).

-opaque t() :: reference().

-type mode() :: generic | text | font.
-type ndirect() :: non_neg_integer().
-type options() :: #{
                   mode => mode(),
                   quality => 0..11,
                   window => 10..30,
                   block_size => 16..24,
                   literal_context_modeling => boolean(),
                   size_hint => non_neg_integer(),
                   large_window => boolean(),
                   npostfix => 0..3,
                   ndirect => ndirect(),
                   stream_offset => non_neg_integer()
                  }.

-spec new() -> t().
new() ->
    brotli_nif:encoder_create().

-spec new(Opts :: options()) -> t().
new(Opts) when is_map(Opts) ->
    Encoder = new(),
    set_opts(Encoder, Opts),
    Encoder.

-spec set_opts(Encoder :: t(), Opts :: options()) -> ok.
set_opts(Encoder, Opts) when is_map(Opts) ->
    maps:foreach(fun(Key, Value) ->
                         true = brotli_nif:encoder_set_parameter(Encoder, Key, Value)
                 end, Opts),
    ok.

-spec append(Encoder :: t(), Data :: iodata()) -> {ok, iodata()} | error.
append(Encoder, Data) ->
    brotli_nif:encoder_compress_stream(Encoder, process, Data).

-spec finish(Encoder :: t()) -> {ok, iodata()} | error.
finish(Encoder) ->
    finish(Encoder, []).

-spec finish(Encoder :: t(), Data :: iodata()) -> {ok, iodata()} | error.
finish(Encoder, Data) ->
    case brotli_nif:encoder_compress_stream(Encoder, finish, Data) of
        ok ->
            {ok, brotli_nif:encoder_take_output(Encoder)};
        error ->
            error
    end.
