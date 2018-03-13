%% @doc Tests for the gateway_geo_gateway_forwarding_scheduler.

-module(geo_gateway_forwarding_scheduler_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

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
