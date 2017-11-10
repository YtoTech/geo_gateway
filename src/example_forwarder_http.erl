%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in dedicated processes.

-module(example_forwarder_http).
-author('yoan@ytotech.com').
% TODO https://stackoverflow.com/questions/32336854/how-to-create-and-use-a-custom-erlang-behavior
% -behaviour(forwarder).

%% API
-export([forward/5]).

-spec forward(Reference :: binary(), Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward(_Reference, Payloads, User, Device, Forwarder) ->
	% TODO Do forward in another process.
	% Basically add to a queue using Erlang messages.
	% Create one new process per forwarding?
	% TODO Really allows meta-programming of an HTTP request through the
	% configuration file. Maybe for that switch to an Erlang configuration
	% file so we can really meta-program.
	Method = nested:get([parameters, method], Forwarder),
	Url = nested:get([parameters, url], Forwarder),
	lists:foreach(
		fun (Payload) ->
			% TODO Pass direclty an Erlang function to format URL and/or payload.
			{_, PayloadData} = Payload,
			PayloadDataJson = #{
				% <<"device">> => ApiDevice,
				<<"timestamp">> => case maps:get(timestamp, PayloadData) of
					nil -> <<"nil">>;
					Timestamp -> iso8601:format(Timestamp)
				end,
				<<"latitude">> => maps:get(latitude, PayloadData),
				<<"longitude">> => maps:get(longitude, PayloadData)
			},
			% Json = #{
			% 	<<"text">> => io_lib:format(
			% 		<<"">>,
			% 		[]
			% 	),
			%
			% 	"{ \"text\": \"New sensor message\nUser: ~s\nDevice: ~s\n\n~s\" }"
			% },
			% io:format("Json: ~p~n", [Json]),
			% PayloadHttp = jiffy:encode(Json),
			PayloadHttp = io_lib:format(
			nested:get([parameters, template], Forwarder),
				[jiffy:encode(User), jiffy:encode(Device), jiffy:encode(PayloadDataJson)]
			),
			io:format("HTTP payload to send (~s:~s) trough Hackney: ~s~n", [Method, Url, PayloadHttp]),
			{ok, StatusCode, _, Client} = hackney:request(
			binary_to_atom(Method, utf8),
			Url,
			% TODO Allows to configure.
			[{<<"Content-Type">>, <<"application/json">>}],
			PayloadHttp
			),
			case StatusCode of
				201 ->
					io:format("Sent!~n"),
					ok;
				_ ->
					{ok, Body} = hackney:body(Client),
					io:format("Body: ~p~n", [Body]),
					io:format("StatusCode: ~p~n", [StatusCode]),
					error
			end
		end,
		Payloads
	),
	ok.
