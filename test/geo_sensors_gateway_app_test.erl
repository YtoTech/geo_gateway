%% @doc Tests for geo_sensors_gateway_app.

-module(geo_sensors_gateway_app_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").


load_configuration_test_() ->
	[
		{"Loads configuration from configuration.json file by default",
		fun() ->
			% TODO How to start it using geo_sensors_gateway:start()?
			StartLink = geo_sensors_gateway_sup:start_link(),
			% TODO Get internal state.
			{StartLinkStatus, _} = StartLink,
			?assertEqual(ok, StartLinkStatus)
		end}
	].
