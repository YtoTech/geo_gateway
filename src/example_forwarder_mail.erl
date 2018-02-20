%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in dedicated processes.

-module(example_forwarder_mail).
-author('yoan@ytotech.com').

%% API
-export([forward_one/5]).

-spec forward_one(Reference :: binary(), Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward_one(_Reference, Payload, _User, _Device, Forwarder) ->
	% TODO Do forward in another process.
	% Basically add to a queue using Erlang messages.
	% Create one new process per forwarding?
	SmtpConfiguration = nested:get([parameters, smtp], Forwarder),
	From = nested:get([parameters, from], Forwarder, "geo-sensors-gateway@ytotech.com"),
	lists:foreach(
		fun(Recipient) ->
			io:format("Forward to ~s~n", [Recipient]),
			gen_smtp_client:send(
				{
					From,
					[Recipient],
					% TODO Timestamp.
					io_lib:format(
						"Subject: ~s\r\nFrom: ~s \r\nTo: ~s \r\n\r\nA new message from your device\r\n\r\n~p\r\n\r\nCheers,\r\nyour GeoSensors bot.",
						[
							nested:get([parameters, subject], Forwarder, "GeoSensors event"),
							From,
							Recipient,
							Payload
						]
					)
				},
				[
					{relay, maps:get(<<"hostname">>, SmtpConfiguration)},
					{username, maps:get(<<"username">>, SmtpConfiguration)},
					{password, maps:get(<<"password">>, SmtpConfiguration)}
				]
			)
		end,
		nested:get([parameters, to], Forwarder)
	),
	ok.
