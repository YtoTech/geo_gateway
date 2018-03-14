%% @doc Tests for geo_gateway_app.

-module(geo_gateway_app_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

-define(
	setup_config(F, Config),
	{setup, fun() -> start_config(Config) end, fun stop_config/1, F}
).

%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TESTS DESCRIPTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%

load_configuration_test_() ->
	[
		{"Loads configuration from configuration.json file by default",
		fun() ->
			{StartAppStatus, _} = application:ensure_all_started(geo_gateway),
			?assertEqual(ok, StartAppStatus),
			ok = application:stop(geo_gateway)
		end},
		{"Loads configuration from configuration.tests.json and ensure it matches",
		fun() ->
			ok = application:set_env(geo_gateway, json_configuration_file, "priv/conf/configuration.tests.json", [{persistent, true}]),
			{StartAppStatus, _} = application:ensure_all_started(geo_gateway),
			% TODO Get internal state. ---> test directly the geo_gateway_config module.
			?assertEqual(ok, StartAppStatus),
			ok = application:stop(geo_gateway)
		end},
		{"Fails if the specified configuration file does not exists",
		fun() ->
			ok = application:set_env(geo_gateway, json_configuration_file, "noexist.conf", [{persistent, true}]),
			{StartAppStatus, Reason} = application:ensure_all_started(geo_gateway),
			?assertEqual(error, StartAppStatus),
			{geo_gateway, {{ReasonForApp, _}, _}} = Reason,
			?assertEqual(no_configuration, ReasonForApp)
		end},
		{"Fails if the config specified is void",
		?setup_config(fun() ->
			{StartAppStatus, Reason} = application:ensure_all_started(geo_gateway),
			?assertEqual(error, StartAppStatus),
			{geo_gateway, {{ReasonForApp, _}, _}} = Reason,
			?assertEqual({badmatch,#{}}, ReasonForApp)
		end, #{})}
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
