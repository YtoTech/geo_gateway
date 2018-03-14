%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.

-module(geo_gateway_forwarder_mail).
-author('yoan@ytotech.com').

-behavior(geo_gateway_forwarder).

%% API
-export([forward/5]).

-spec forward(Reference :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward(_Reference, Payloads, _User, _Device, Forwarder) ->
	SmtpConfiguration = nested:get([parameters, smtp], Forwarder),
	From = nested:get([parameters, from], Forwarder, "geo-sensors-gateway@ytotech.com"),
	lists:foreach(
		fun(Recipient) ->
			lager:info("Forward to ~s", [Recipient]),
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
							Payloads
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
