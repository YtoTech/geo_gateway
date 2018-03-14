%% @doc Tests for forwarding of geo_gateway_app.

-module(geo_gateway_forwarding_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

-define(
	setup_config(F, Config),
	{setup, fun() -> start_config(Config) end, fun stop_config/1, F}
).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(
	SAMPLE_CONFIG,
	#{
		users => #{
			<<"annon">> => #{
				email => <<"test@ytotech.com">>,
				password => <<"coincoin">>,
				device => <<"ercogener_genloc_341e">>,
				dumps_incoming => false,
				forwarders => [
					<<"erlang_module_forwarder">>
				]
			}
		},
		devices => #{
			<<"ercogener_genloc_341e">> => #{
				manufacturer => <<"Ercogener">>,
				range => <<"GenLoc">>,
				model => <<"451e EaseLoc">>
			}
		},
		smtp_gateway => #{
			port => 2525
		},
		forwarders => #{
			<<"erlang_module_forwarder">> => #{
				module => <<"geo_gateway_forwarder_module">>,
				parameters => #{
					target_module => <<"test_receiver">>
				}
			}
		}
	}
).

-define(
	SAMPLE_EMAIL,
	{
		"test@ytotech.com", ["receiver@ytotech.com"],
		"Subject: testing\r\nFrom: test@ytotech.com \r\nTo: receiver@ytotech.com \r\n\r\n$GPRMC,163734.00,A,4434.34454,N,00046.44022,E,0.015,0.00,230917,,,A*67"
	}
).

-define(
	TEST_GATEWAY_OPTIONS,
	[{relay, "localhost"}, {username, "annon"}, {password, "coincoin"}, {port, 2525}]
).

-define(
	PAYLOAD_PATTERN,
	{ok, #{
		date := _,
		format := _,
		longitude := _,
		latitude := _,
		raw := _,
		status := _,
		time := _,
		timestamp := _
	}}
).

forwarding_test_() ->
	[
		{"We can forward to a simple test box receiver",
		?setup_config(fun() ->
			{ok, _} = application:ensure_all_started(geo_gateway),
			gen_smtp_client:send_blocking(?SAMPLE_EMAIL, ?TEST_GATEWAY_OPTIONS),
			application:stop(geo_gateway),
			ReceivedPayloads = test_receiver:get_received_payloads(),
			?assertEqual(1, length(ReceivedPayloads)),
			?assertMatch([?PAYLOAD_PATTERN], ReceivedPayloads)
		end, ?SAMPLE_CONFIG)}
	].

forwarding_scheduler_test_() ->
	{timeout, 15,
	[
		{"All payloads are finally forwarded in presence of transient 25 % drop-rate",
		?setup_config(fun() ->
			{ok, _} = application:ensure_all_started(geo_gateway),
			lists:foreach(
				fun(_Index) ->
					% TODO Use async and wait for all only at the end?
					gen_smtp_client:send_blocking(?SAMPLE_EMAIL, ?TEST_GATEWAY_OPTIONS)
				end,
				lists:seq(1, 100)
			),
			application:stop(geo_gateway),
			ReceivedPayloads = test_receiver:get_received_payloads(),
			?assertEqual(100, length(ReceivedPayloads)),
			lists:foreach(
				fun(ReceivedPayload) ->
					?assertMatch(?PAYLOAD_PATTERN, ReceivedPayload)
				end,
				ReceivedPayloads
			)
		end, maps:merge(?SAMPLE_CONFIG, #{
			forwarders => #{
				<<"erlang_module_forwarder">> => #{
					module => <<"geo_gateway_forwarder_module">>,
					parameters => #{
						target_module => <<"test_receiver">>,
						drop_strategy => random,
						drop_rate => 0.20
					}
				}
			}
		}))}
	]}.

% TODO Test for abort with 99 or 100 drop-rate --> reschedule N times and finally surrender ;
% and app killed without proper termination.
% Will need mocking or configuring the timeout delay to trigger it?
% (Or else will be way too long?)
% If we need mocking: https://github.com/eproxus/meck

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_config(Config) ->
	{ok, _} = geo_gateway_config_loader_in_memory:start_link(),
	ok = geo_gateway_config_loader_in_memory:set_config(Config),
	ok = application:set_env(geo_gateway, gateway_config_loader, "geo_gateway_config_loader_in_memory", [{persistent, true}]),
	{ok, _} = test_receiver:start_link(),
	undefined.

stop_config(_) ->
	ok = geo_gateway_config_loader_in_memory:stop(),
	ok = test_receiver:stop().
