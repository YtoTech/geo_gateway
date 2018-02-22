%% @doc Tests for geo_sensors_gateway_app.

-module(geo_sensors_gateway_app_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

% TODO Create setup and teardown to start and stop app between tests.
% Also allows to pass parameters to app before starting it.
% (Using application:set_env())

load_configuration_test_() ->
	[
		{"Loads configuration from configuration.json file by default",
		fun() ->
			{StartAppStatus, _} = application:ensure_all_started(geo_sensors_gateway),
			?assertEqual(ok, StartAppStatus),
			ok = application:stop(geo_sensors_gateway)
		end},
		{"Loads configuration from configuration.json.tests and ensure it matches",
		fun() ->
			ok = application:set_env(geo_sensors_gateway, json_configuration_file, "priv/conf/configuration.json.tests", [{persistent, true}]),
			{StartAppStatus, _} = application:ensure_all_started(geo_sensors_gateway),
			% TODO Get internal state. ---> test directly the geo_sensors_gateway_config module.
			?assertEqual(ok, StartAppStatus),
			ok = application:stop(geo_sensors_gateway)
		end},
		{"Fails if the specified configuration file does not exists",
		fun() ->
			ok = application:set_env(geo_sensors_gateway, json_configuration_file, "noexist.conf", [{persistent, true}]),
			{StartAppStatus, Reason} = application:ensure_all_started(geo_sensors_gateway),
			?assertEqual(error, StartAppStatus),
			{geo_sensors_gateway, {{ReasonForApp, _}, _}} = Reason,
			?assertEqual(no_configuration, ReasonForApp)
		end}
	].

% TODO Test for forwarding fault-tolerance:
% * launch a geo_sensors_gateway application, with one forwarder ;
% * implement this forwarder as to fail to forward randomly half or 25 % of the payloads ;
% * implement the receiver as a simple process getting the messages;
% * all messages must have been correctly forwarder to the receiver at the end of the test.
