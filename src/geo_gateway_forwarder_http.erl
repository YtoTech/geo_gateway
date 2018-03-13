%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in dedicated processes.

-module(geo_gateway_forwarder_http).
-author('yoan@ytotech.com').

%% API
-export([forward_one/5]).

-spec forward_one(Reference :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward_one(_Reference, Payloads, User, Device, Forwarder) ->
	% TODO Do forward in another process.
	% Basically add to a queue using Erlang messages.
	% Create one new process per forwarding?
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
	% PayloadHttp = jiffy:encode(Json),
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
			lager:info("Sent!"),
			ok;
		_ ->
			{ok, Body} = hackney:body(Client),
			lager:warning("Body: ~p", [Body]),
			lager:warning("StatusCode: ~p", [StatusCode]),
			error
	end.

erlang_to_json_string(Erlang) ->
	string:replace(io_lib:format("~p", [Erlang]), "\"", "\\\"", all).
