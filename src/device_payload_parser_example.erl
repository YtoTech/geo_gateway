%% @doc A simple example callback module for `device_payload_parser' that also serves as
%% documentation for the required callback API.
%% Responsible for parsing sensor paylods and extract all usefull information
%% from them.
%% This data will then be transferred to any endpoint by forwarders.

-module(device_payload_parser_example).
-author('yoan@ytotech.com').
% TODO https://stackoverflow.com/questions/32336854/how-to-create-and-use-a-custom-erlang-behavior
% -behaviour(device_payload_parser).

%% API
-export([parse/3]).

-spec parse(Body :: binary(), User :: map(), Devices :: map()) -> {'ok', map()} | {'error', string()}.
parse(Body, User, Devices) ->
	% Get the sensor type from user config
	% and transfert to the appropriate parsing module.
	case maps:find(maps:get(device, User), Devices) of
		{ok, Device} ->
			case Device of
				#{manufacturer := <<"Ercogener">>, range := <<"GenLoc">>} ->
					io:format("Erco"),
					{ok, Body}
			end;
		_ ->
			io:format("No device for user: ignore"),
			erlang:error(no_device, "No device for user")
	end.
