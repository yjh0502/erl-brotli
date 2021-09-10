%%% This file is part of the brotli distribution (https://github.com/hauleth/erl-brotli).
%%% Copyright (c) 2021 Łukasz Niemier <lukasz@niemier.pl>.
%%%
%%% This program is free software: you can redistribute it and/or modify  
%%% it under the terms of the GNU General Public License as published by  
%%% the Free Software Foundation, version 3.
%%%
%%% This program is distributed in the hope that it will be useful, but 
%%% WITHOUT ANY WARRANTY; without even the implied warranty of 
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU 
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License 
%%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(prop_brotli).

-include_lib("proper/include/proper.hrl").

prop_encoded_can_be_decoded() ->
    ?FORALL(Data, binary(), begin
                                {ok, Encoded} = brotli:encode(Data),
                                {ok, Decoded} = brotli:decode(Encoded),
                                Data =:= Decoded
                            end).

prop_encoded_data_can_be_decoded_partially() ->
    ?FORALL(Data, binary(), begin
                                {ok, Encoded} = brotli:encode(Data),
                                Decoder = brotli_decoder:new(),
                                Decoded = [element(2, brotli_decoder:stream(Decoder, <<B>>)) || <<B>> <= Encoded],
                                Data =:= iolist_to_binary(Decoded)
                            end).

prop_encoding_all_at_once_and_byte_by_byte_is_equivalent() ->
    ?FORALL(Data, binary(), begin
                                Encoder = brotli_encoder:new(),
                                Encoded = [element(2, brotli_encoder:append(Encoder, <<B>>)) || <<B>> <= Data],
                                {ok, Last} = brotli_encoder:finish(Encoder),
                                brotli:encode(Data) =:= {ok, iolist_to_binary([Encoded, Last])}
                            end).

prop_encoded_is_not_bigger_than_max_compressed_size() ->
    ?FORALL(Data, binary(), begin
                                {ok, Encoded} = brotli:encode(Data),
                                byte_size(Encoded) =< brotli:max_compressed_size(byte_size(Data))
                            end).
