%% @doc Tests for geo_sensors_gateway_app.

-module(geo_sensors_gateway_app_test).
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
		end},
		{"Fails if the config specified is void",
		?setup_config(fun() ->
			{StartAppStatus, Reason} = application:ensure_all_started(geo_sensors_gateway),
			?assertEqual(error, StartAppStatus),
			{geo_sensors_gateway, {{ReasonForApp, _}, _}} = Reason,
			?assertEqual({badmatch,#{}}, ReasonForApp)
		end, #{})}
	].

forwarding_test_() ->
	[
		{"We can forward to a simple test box receiver",
		?setup_config(fun() ->\
			{ok, _} = application:ensure_all_started(geo_sensors_gateway)
			% TODO No just send a sample payload to the server. (Use a true SMTP send to 2525)
		end, #{
			users => #{},
			devices => #{
				"ercogener_genloc_341e" => #{
					"manufacturer" => "Ercogener",
					"range" => "GenLoc",
					"model" => "451e EaseLoc"
				}
			},
			smtp_gateway => #{
				port => 2525
			},
			forwarders => #{
				% TODO Actually use a simple module forwarder / test simple module forwarder.
				% (So we can specify options as receiver module, drop ratio, etc.).
				"file_dump" => #{
					"module" => "example_forwarder_file_dump",
					"parameters" => #{
						"path" => "dumps/"
					}
				}
			}
		})}
	].

% TODO Test for forwarding fault-tolerance:
% * launch a geo_sensors_gateway application, with one forwarder ;
% * implement this forwarder as to fail to forward randomly half or 25 % of the payloads ;
% * implement the receiver as a simple process getting the messages; ---> a gen_server with receive / get_all.
% * all messages must have been correctly forwarder to the receiver at the end of the test.

% If we need mocking: https://github.com/eproxus/meck

%%%%%%%%%%%%%%%%%%%%%%%
%%% SETUP FUNCTIONS %%%
%%%%%%%%%%%%%%%%%%%%%%%
start_config(Config) ->
	{ok, _} = gateway_config_loader_process_dict:start_link(),
	ok = gateway_config_loader_process_dict:set_config(Config),
	ok = application:set_env(geo_sensors_gateway, gateway_config_loader, "gateway_config_loader_process_dict", [{persistent, true}]),
	undefined.

stop_config(_) ->
	ok = gateway_config_loader_process_dict:stop().
