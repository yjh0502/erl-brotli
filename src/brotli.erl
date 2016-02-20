-module('brotli').

-export([encode/1, encode/2]).

-define(DEFAULT_QUALITY, 6).

encode(Data) ->
    brotli_nif:brotli_encode(Data, ?DEFAULT_QUALITY).

encode(Data, Quality) ->
    brotli_nif:brotli_encode(Data, Quality).
