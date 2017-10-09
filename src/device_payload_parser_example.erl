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
					{ok, ParsedPayload} = parse_genloc(Body, User, Device),
					{ok, ParsedPayload, Device};
				_ ->
					io:format("No parser for device ~p: ignore~n", [Device]),
					{error, no_device_parser}
			end;
		_ ->
			io:format("No device for user: ignore~n"),
			{error, no_device}
	end.

parse_genloc(Body, User, Device) ->
	io:format("~p~n", [Body]),
	% Prepare input: split by lines and trim them.
	Lines = string:split(Body, "\n", all),
	TrimedLines = lists:map(fun (Line) -> string:trim(Line) end, Lines),
	% We consider only (non-empty) lines.
	PayloadLines = lists:filtermap(
		fun (Line) -> not string:is_empty(Line) end,
		TrimedLines
	),
	% TODO Create meta-mapping for parsing NMEA trames.
	% Or use http://nmea.io/ with a NIF.
	% Implement custom parsers.
	% Parsing in C is ok if it return fast and return an error / null when
	% the parsing fail.
	% It is in any case a good idea to code these trame parsing from meta
	% configuration that describe the format, as it is basically pair
	% of key-value.
	% --> So we can make it configurable.
	io:format("~p~n", [PayloadLines]),
	Payload = lists:map(
		fun (Line) ->
			io:format("Line: ~s~n", [Line]),
			% case string:find(Line, "$GPLOC") of
			% 	nomatch -> false;
			% 	_ -> true
			% end
			case Line of
				Line when string:find(Line, "$GPLOC") ->
					io:format("GPLOC: ~s~n", [Str]),
					ok;
				_ -> momatch
			end
		end,
		PayloadLines
	),
	io:format("~p~n", [Payload]),
	{ok, Payload}.
