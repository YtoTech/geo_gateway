%% @doc Tests for geo_sensors_gateway_app.

-module(geo_sensors_gateway_app_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").


load_configuration_test_() ->
	[
		{"Loads configuration from configuration.json file by default",
		fun() ->
			{StartAppStatus, _} = application:ensure_all_started(geo_sensors_gateway),
			% TODO Get internal state.
			?assertEqual(ok, StartAppStatus)
		end}
	].

% TODO Test for forwarding fault-tolerance:
% * launch a geo_sensors_gateway application, with one forwarder ;
% * implement this forwarder as to fail to forward randomly half or 25 % of the payloads ;
% * implement the receiver as a simple process getting the messages;
% * all messages must have been correctly forwarder to the receiver at the end of the test.
