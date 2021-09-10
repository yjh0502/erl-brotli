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
-export([tool_encode/1]).

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

prop_encoded_is_the_same_as_from_tool() ->
    ?FORALL(Data, binary(), begin
                                {ok, EncodedErl} = brotli:encode(Data,
                                                                 #{window => 22}),
                                {ok, EncodedTool} = tool_encode(Data),
                                EncodedErl =:= EncodedTool
                            end).

prop_can_decode_data_encoded_by_tool() ->
    ?FORALL(Data, binary(), begin
                                {ok, Encoded} = tool_encode(Data),
                                {ok, Decoded} = brotli:decode(Encoded),
                                Decoded =:= Data
                            end).

tool_encode(Data) ->
    Hash = to_hex(erlang:md5(Data)),
    TmpPath = filename:join(temp_dir(), "erl-brotli"),
    file:make_dir(TmpPath),
    Path = filename:join(TmpPath, Hash),
    ok = file:write_file(Path, Data, [binary, raw, write]),
    PrivDir = code:priv_dir(brotli),
    Tool = filename:join(PrivDir, "brotli"),
    Port = open_port({spawn_executable, Tool}, [{args, ["-cfw", "22", "--", Path]},
                                                exit_status, use_stdio,
                                                binary]),
    {Result, 0} = receive_all(Port, []),
    {ok, Result}.

temp_dir() ->
    Envs = ["TEMPDIR", "TMPDIR", "TEMP", "TMP"],
    find_value(Envs).

find_value([]) ->
    "/tmp";
find_value([Env | Rest]) ->
    case os:getenv(Env) of
        false -> find_value(Rest);
        Path -> Path
    end.

to_hex(Bin) ->
    << <<Y>> || <<X:4>> <= Bin, Y <- integer_to_list(X,16) >>.

receive_all(Port, Acc) ->
    receive
        {Port, {data, Data}} -> receive_all(Port, [Data | Acc]);
        {Port, {exit_status, Status}} -> {iolist_to_binary(lists:reverse(Acc)), Status}
    end.
