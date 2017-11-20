%% @doc Tests for geo_sensors_gateway_app.

-module(geo_sensors_gateway_app_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").


load_configuration_test_() ->
	[
		{"Loads configuration from configuration.json file by default",
		fun() ->
			% Start all app we required on.
			% TODO There must be a way to load all that is required.
			lists:foreach(
				fun(AppName) ->
					ok = application:start(AppName)
				end,
				[eiconv, jiffy, gen_smtp, nested, idna, unicode_util_compat, hackney]
			),
			% TODO How to start it using geo_sensors_gateway:start()?
			StartApp = application:start(geo_sensors_gateway),
			% TODO Get internal state.
			?assertEqual(ok, StartApp)
		end}
	].
