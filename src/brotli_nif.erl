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

%% @private

-module(brotli_nif).

-on_load(init/0).

-define(APPNAME, brotli).
-define(LIBNAME, brotli).

-export([
    encoder_create/0,
    encoder_set_parameter/3,
    encoder_compress_stream/3,
    encoder_has_more_output/1,
    encoder_is_finished/1,
    encoder_take_output/1
]).
-export([
    decoder_create/0,
    decoder_decompress_stream/2,
    decoder_has_more_output/1,
    decoder_is_finished/1,
    decoder_is_used/1,
    decoder_take_output/1,
    decoder_error_description/1
]).
-export([max_compressed_size/1, version/0]).

%%%
init() ->
    SoName =
        case code:priv_dir(?APPNAME) of
            {error, bad_name} ->
                case filelib:is_dir(filename:join(["..", priv])) of
                    true ->
                        filename:join(["..", priv, ?LIBNAME]);
                    _ ->
                        filename:join([priv, ?LIBNAME])
                end;
            Dir ->
                filename:join(Dir, ?LIBNAME)
        end,
    erlang:load_nif(SoName, 0).

%%% Exported from brotli_nif.c.

encoder_create() ->
    erlang:nif_error(not_loaded).

encoder_set_parameter(_Encoder, _Param, _Value) ->
    erlang:nif_error(not_loaded).

encoder_compress_stream(_Encoder, _Op, _Value) ->
    erlang:nif_error(not_loaded).

encoder_has_more_output(_Encoder) ->
    erlang:nif_error(not_loaded).

encoder_is_finished(_Encoder) ->
    erlang:nif_error(not_loaded).

encoder_take_output(_Encoder) ->
    erlang:nif_error(not_loaded).

decoder_create() ->
    erlang:nif_error(not_loaded).

decoder_decompress_stream(_Encoder, _Value) ->
    erlang:nif_error(not_loaded).

decoder_has_more_output(_Encoder) ->
    erlang:nif_error(not_loaded).

decoder_is_finished(_Encoder) ->
    erlang:nif_error(not_loaded).

decoder_is_used(_Encoder) ->
    erlang:nif_error(not_loaded).

decoder_take_output(_Encoder) ->
    erlang:nif_error(not_loaded).

decoder_error_description(_Encoder) ->
    erlang:nif_error(not_loaded).

max_compressed_size(_Size) ->
    erlang:nif_error(not_loaded).

version() ->
    erlang:nif_error(not_loaded).
