%% @doc A simple example callback module for `device_payload_parser' that also serves as
%% documentation for the required callback API.
%% Responsible for parsing sensor payloads and extract all usefull information
%% from them.
%% This data will then be transferred to any endpoint by forwarders.

-module(device_payload_parser_example).
-author('yoan@ytotech.com').

-behaviour(device_payload_parser).

%% API
-export([parse/4]).

-spec parse(Reference :: binary(), Body :: binary(), User :: map(), Devices :: map()) -> {'ok', list(), map()} | {'error', atom()}.
parse(_Reference, Body, User, Devices) ->
	% Get the sensor type from user config
	% and transfert to the appropriate parsing module.
	case maps:find(maps:get(device, User), Devices) of
		{ok, Device} ->
			case Device of
				#{manufacturer := <<"Ercogener">>, range := <<"GenLoc">>} ->
					{ok, ParsedPayload} = parse_genloc(Body, User, Device),
					{ok, ParsedPayload, Device};
				_ ->
					lager:warning("No parser for device ~p: ignore", [Device]),
					{error, no_device_parser}
			end;
		_ ->
			lager:warning("No device for user: ignore"),
			{error, no_device}
	end.

-spec parse_genloc(Body :: binary(), User :: map(), Device :: map()) -> {'ok', list()}.
parse_genloc(Body, _User, _Device) ->
	lager:debug("~p", [Body]),
	% Prepare input: split by lines and trim them.
	Lines = string:split(Body, <<"\n">>, all),
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
	% Also Python alternative: https://github.com/Knio/pynmea2
	% Other way is to create a micro-HTTP service dedicated to the task.
	% --> This can help others and may be a service for geo-gateway.com.
	lager:debug("~p", [PayloadLines]),
	Payload = lists:map(
		fun (Line) ->
			lager:debug("Line: ~s~n", [Line]),
			case string:split(Line, <<",">>) of
				[<<"$GPLOC">>,_] ->
					parse_nmea(<<"GPLOC">>, Line);
				[<<"$GPRMC">>,_] ->
					parse_nmea(<<"GPRMC">>, Line);
				_ ->
					lager:error("momatch: ~p", [string:split(Line, ",")]),
					{momatch, Line}
			end
		end,
		PayloadLines
	),
	lager:debug("~p", [Payload]),
	{ok, Payload}.

-spec parse_nmea(Type :: binary(), Line :: binary()) -> {'ok', map()}.
parse_nmea(<<"GPLOC">>, Line) ->
	% TODO The GPLOC NMEA is configurable on the sensor and thus its format is
	% variable. Make the expected trame format configurable for each device.
	% For the moment, we try to guess.
	lager:debug("GPLOC: ~s", [Line]),
	Splitted = string:split(Line, <<",">>, all),
	Elements = array:from_list(Splitted),
	{Offset, _Identifier} = case array:get(1, Elements) of
		Status when Status =:= <<"A">>; Status =:= <<"V">> ->
		% <<"A">> ->
			{0, undef};
		Identifier ->
			{1, Identifier}
	end,
	Time = parse_time_hhmmssmm(array:get(3 + Offset, Elements)),
	{ok, #{
		format => <<"GPLOC">>,
		status => array:get(1 + Offset, Elements),
		fix_quality => array:get(2 + Offset, Elements),
		time => Time,
		% TODO Get a normalized timestamp.
		% How to determine timestamp? We can not be sure the trame is
		% from today.
		date => nil,
		timestamp => nil,
		latitude => geo_conversions:sexagesimal_to_decimal(array:get(4 + Offset, Elements), array:get(5 + Offset, Elements)),
		longitude => geo_conversions:sexagesimal_to_decimal(array:get(6 + Offset, Elements), array:get(7 + Offset, Elements)),
		raw => Line
	}};
parse_nmea(<<"GPRMC">>, Line) ->
	lager:debug("GPRMC: ~s", [Line]),
	Splitted = string:split(Line, <<",">>, all),
	Elements = array:from_list(Splitted),
	Date = parse_date_ddmmyy(array:get(9, Elements)),
	Time = parse_time_hhmmssmm(array:get(1, Elements)),
	{ok, #{
		format => <<"GPRMC">>,
		time => Time,
		date => Date,
		% Get a normalized timestamp.
		timestamp => {
			Date,
			Time
		},
		status => array:get(2, Elements),
		latitude => geo_conversions:sexagesimal_to_decimal(array:get(3, Elements), array:get(4, Elements)),
		longitude => geo_conversions:sexagesimal_to_decimal(array:get(5, Elements), array:get(6, Elements)),
		raw => Line
	}}.


parse_date_ddmmyy(Date) ->
	% TODO Stuck in 20XX? Please see the past and future.
	{Day, _} = string:to_integer(string:slice(Date, 0, 2)),
	{Month, _} = string:to_integer(string:slice(Date, 2, 2)),
	{Year, _} = string:to_integer(erlang:iolist_to_binary([<<"20">>, string:slice(Date, 4)])),
	{Year, Month, Day}.

parse_time_hhmmssmm(Time) ->
	{Hours, _} = string:to_integer(string:slice(Time, 0, 2)),
	{Minutes, _} = string:to_integer(string:slice(Time, 2, 2)),
	% Get mm?
	{Seconds, _} = string:to_float(string:slice(Time, 4)),
	{Hours, Minutes, Seconds}.
