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

-module(brotli_decoder).

-export([new/0, stream/2]).
-export([is_finished/1, is_used/1, has_more_output/1]).

-export_type([t/0]).

-opaque t() :: reference().

-spec new() -> t().
new() ->
    brotli_nif:decoder_create().

-spec stream(Decoder :: t(), Data :: iodata()) -> {ok | more, iodata()} | error.
stream(Decoder, Data) ->
    case brotli_nif:decoder_decompress_stream(Decoder, Data) of
        ok ->
            {ok, brotli_nif:decoder_take_output(Decoder)};
        more ->
            {more, brotli_nif:decoder_take_output(Decoder)};
        Other ->
            Other
    end.

is_finished(Decoder) ->
    brotli_nif:decoder_is_finished(Decoder).

is_used(Decoder) ->
    brotli_nif:decoder_is_used(Decoder).

has_more_output(Decoder) ->
    brotli_nif:decoder_has_more_output(Decoder).
