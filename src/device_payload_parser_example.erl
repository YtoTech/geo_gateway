%% @doc A simple example callback module for `device_payload_parser' that also serves as
%% documentation for the required callback API.
%% Responsible for parsing sensor payloads and extract all usefull information
%% from them.
%% This data will then be transferred to any endpoint by forwarders.

-module(device_payload_parser_example).
-author('yoan@ytotech.com').
% TODO https://stackoverflow.com/questions/32336854/how-to-create-and-use-a-custom-erlang-behavior
% -behaviour(device_payload_parser).

%% API
-export([parse/3]).

-spec parse(Body :: binary(), User :: map(), Devices :: map()) -> {'ok', map(), map()} | {'error', atom()}.
parse(Body, User, Devices) ->
	% Get the sensor type from user config
	% and transfert to the appropriate parsing module.
	case maps:find(maps:get(device, User), Devices) of
		{ok, Device} ->
			case Device of
				#{manufacturer := <<"Ercogener">>, range := <<"GenLoc">>} ->
					io:format("Erco"),
					{ok, Body, Device};
				_ ->
					io:format("No parser for device ~p: ignore~n", [Device]),
					{error, no_device_parser}
			end;
		_ ->
			io:format("No device for user: ignore~n"),
			{error, no_device}
	end.
