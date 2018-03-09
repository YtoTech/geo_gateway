%% @doc A utilitary module to convert geo coordinates.
% TODO Create a library of it.

-module(geo_conversions).
-author('yoan@ytotech.com').

-export([sexagesimal_to_decimal/2, ddm_to_decimal/2, dms_to_decimal/2]).

% TODO Create a library for doing geo conversion in Erlang.
% https://github.com/manuelbieh/Geolib#geolibsexagesimal2decimalstring-coord
% TODO Manage error cases.
-spec sexagesimal_to_decimal(Sexagesimal :: binary(), Cardinality :: binary()) -> float() | nil.
sexagesimal_to_decimal(<<"">>, <<"">>) ->
	nil;
sexagesimal_to_decimal(Sexagesimal, Cardinality) ->
	% TODO How to switch between DDM and DMS?
	ddm_to_decimal(Sexagesimal, Cardinality).

ddm_to_decimal(Sexagesimal, Cardinality) ->
	lager:debug("Sexagesimal: ~p Cardinality: ~p", [Sexagesimal,Cardinality]),
	% Longitude: 2 digits for degree (0 to 89). Latitude: 3 digits for degree (0 to 179).
	DegreeNumberDigits = case Cardinality of
		<<"S">> -> 2;
		<<"N">> -> 2;
		<<"W">> -> 3;
		<<"E">> -> 3
	end,
	{Degrees, _} = string:to_integer(string:slice(Sexagesimal, 0, DegreeNumberDigits)),
	{Minutes, _} = string:to_float(string:slice(Sexagesimal, DegreeNumberDigits)),
	lager:debug("Degrees: ~p Minutes: ~p", [Degrees,Minutes]),
	DecimalNoCardinality = Degrees + Minutes / 60,
	case Cardinality of
		<<"S">> -> -DecimalNoCardinality;
		<<"W">> -> -DecimalNoCardinality;
		<<"N">> -> DecimalNoCardinality;
		<<"E">> -> DecimalNoCardinality
	end.

dms_to_decimal(Sexagesimal, Cardinality) ->
	% TODO Broken. Should parse 44°34'20.7"N 0°46'26.4"E
	lager:debug("Sexagesimal: ~p Cardinality: ~p", [Sexagesimal,Cardinality]),
	[Ddmm, Mmmmm] = string:split(Sexagesimal, <<".">>),
	{Degrees, _} = string:to_integer(string:slice(Ddmm, 0, 2)),
	{Minutes, _} = string:to_integer(string:slice(Ddmm, 2)),
	{Secondes, _} = string:to_float(string:slice(Mmmmm, 0, 2)),
	lager:debug("Degrees: ~p Minutes: ~p Secondes: ~p", [Degrees,Minutes,Secondes]),
	DecimalNoCardinality = Degrees + Minutes / 60 + Secondes / 3600,
	case Cardinality of
		<<"S">> -> -DecimalNoCardinality;
		<<"W">> -> -DecimalNoCardinality;
		<<"N">> -> DecimalNoCardinality;
		<<"E">> -> DecimalNoCardinality
	end.
