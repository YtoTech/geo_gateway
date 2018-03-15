%% @doc Tests for the gateway_geo_gateway_forwarding_scheduler.

-module(geo_gateway_forwarding_scheduler_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

-define(
	setup_config(F, Config),
	{setup, fun() -> start_config(Config) end, fun stop_config/1, F}
).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

backoff_delay_test_() ->
	[
		{"Test backoff delay strategy",
		[
			?_assertEqual(500, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 1 })),
			?_assertEqual(1000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 2 })),
			?_assertEqual(2000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 3 })),
			?_assertEqual(4000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 4 })),
			?_assertEqual(8000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 5 })),
			?_assertEqual(16000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 6 })),
			?_assertEqual(32000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 7 })),
			?_assertEqual(64000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 8 })),
			?_assertEqual(120000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 9 })),
			?_assertEqual(120000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 10 })),
			?_assertEqual(120000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 11 })),
			?_assertEqual(120000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 12 })),
			?_assertEqual(120000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 13 })),
			?_assertMatch(120000, geo_gateway_forwarding_scheduler:reschedule_compute_delay(#{ retries => 14 }))
		]}
	].

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

scheduler_sup_test_() ->
	[
		{"Scheduler should restart if it is killed",
		?setup_config(fun() ->
			{ok, _} = application:ensure_all_started(geo_gateway),
			% Kill the geo_gateway_forwarding_scheduler process.
			PidBefore = whereis(geo_gateway_forwarding_scheduler),
			PidSupBefore = whereis(geo_gateway_forwarding_scheduler_sup),
			exit(whereis(geo_gateway_forwarding_scheduler), violent),
			timer:sleep(20),
			PidAfter = whereis(geo_gateway_forwarding_scheduler),
			PidSupAfter = whereis(geo_gateway_forwarding_scheduler_sup),
			gen_smtp_client:send_blocking(?SAMPLE_EMAIL, ?TEST_GATEWAY_OPTIONS),
			application:stop(geo_gateway),
			ReceivedPayloads = test_receiver:get_received_payloads(),
			?assertEqual(1, length(ReceivedPayloads)),
			?assertNotEqual(PidBefore, PidAfter),
			?assertNotEqual(PidSupBefore, PidSupAfter)
		end, ?SAMPLE_CONFIG)}
	].

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
