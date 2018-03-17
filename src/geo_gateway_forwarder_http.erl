%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.

-module(geo_gateway_forwarder_http).
-author('yoan@ytotech.com').

-behavior(geo_gateway_forwarder).
% TODO Also implement for forward_one.
% -behavior(geo_gateway_forwarder_one).

%% API
-export([forward/5, float_format/0, json_options/0]).

-spec forward(Reference :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward(_Reference, Payloads, User, Device, Forwarder) ->
	% TODO Really allows meta-programming of an HTTP request through the
	% configuration file. Maybe for that switch to an Erlang configuration
	% file so we can really meta-program.
	Method = nested:get([parameters, method], Forwarder),
	Url = nested:get([parameters, url], Forwarder),
	% % TODO Pass direclty an Erlang function to format URL and/or payload.
	% {_, PayloadData} = Payload,
	% PayloadDataJson = #{
	% 	% <<"device">> => ApiDevice,
	% 	<<"timestamp">> => case maps:get(timestamp, PayloadData) of
	% 		nil -> <<"nil">>;
	% 		Timestamp -> iso8601:format(Timestamp)
	% 	end,
	% 	<<"latitude">> => maps:get(latitude, PayloadData),
	% 	<<"longitude">> => maps:get(longitude, PayloadData)
	% },
	% Json = #{
	% 	<<"text">> => io_lib:format(
	% 		<<"">>,
	% 		[]
	% 	),
	%
	% 	"{ \"text\": \"New sensor message\nUser: ~s\nDevice: ~s\n\n~s\" }"
	% },
	% lager:debug("Json: ~p", [Json]),
	% PayloadHttp = jsone:encode(Json, json_options()),
	PayloadHttp = io_lib:format(
		nested:get([parameters, template], Forwarder),
		[erlang_to_json_string(User), erlang_to_json_string(Device), erlang_to_json_string(Payloads)]
	),
	lager:info("HTTP payload to send (~s:~s): ~s", [Method, Url, PayloadHttp]),
	{ok, StatusCode, _, Client} = hackney:request(
	binary_to_atom(Method, utf8),
	Url,
	% TODO Allows to configure.
	[{<<"Content-Type">>, <<"application/json">>}],
	PayloadHttp
	),
	case StatusCode of
		% TODO 20X
		200 ->
			lager:info("HTTP forward ok ~p", [Url]),
			ok;
		_ ->
			{ok, Body} = hackney:body(Client),
			lager:warning("Body: ~p", [Body]),
			lager:warning("StatusCode: ~p", [StatusCode]),
			error
	end.

erlang_to_json_string(Erlang) ->
	string:replace(io_lib:format("~p", [Erlang]), "\"", "\\\"", all).

float_format() ->
	[{decimals, 15}, compact].

json_options() ->
	[{float_format, float_format()}].

format_float_to_string(Float) ->
	float_to_binary(Float, geo_gateway_forwarder_http:float_format()).
