%% @doc Tests for the gateway_forwarding_scheduler.

-module(forwarding_scheduler_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

backoff_delay_test_() ->
	[
		{"Test backoff delay strategy",
		[
			?_assertEqual(500, forwarding_server:reschedule_compute_delay(#{ retries => 1 })),
			?_assertEqual(1000, forwarding_server:reschedule_compute_delay(#{ retries => 2 })),
			?_assertEqual(2000, forwarding_server:reschedule_compute_delay(#{ retries => 3 })),
			?_assertEqual(4000, forwarding_server:reschedule_compute_delay(#{ retries => 4 })),
			?_assertEqual(8000, forwarding_server:reschedule_compute_delay(#{ retries => 5 })),
			?_assertEqual(16000, forwarding_server:reschedule_compute_delay(#{ retries => 6 })),
			?_assertEqual(32000, forwarding_server:reschedule_compute_delay(#{ retries => 7 })),
			?_assertEqual(64000, forwarding_server:reschedule_compute_delay(#{ retries => 8 })),
			?_assertEqual(120000, forwarding_server:reschedule_compute_delay(#{ retries => 9 })),
			?_assertEqual(120000, forwarding_server:reschedule_compute_delay(#{ retries => 10 })),
			?_assertEqual(120000, forwarding_server:reschedule_compute_delay(#{ retries => 11 })),
			?_assertEqual(120000, forwarding_server:reschedule_compute_delay(#{ retries => 12 })),
			?_assertEqual(120000, forwarding_server:reschedule_compute_delay(#{ retries => 13 })),
			?_assertMatch(120000, forwarding_server:reschedule_compute_delay(#{ retries => 14 }))
		]}
	].
