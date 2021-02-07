%%%-------------------------------------------------------------------
%% @doc geo_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(geo_gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	lager:debug("Env is ~p", [application:get_all_env()]),
	% Load application configuration.
	lager:debug("Config loader is ~p", [application:get_env(gateway_config_loader)]),
	{ok, ConfigLoaderName} = application:get_env(gateway_config_loader),
	ConfigLoader = list_to_atom(ConfigLoaderName),
	{module, _} = code:ensure_loaded(ConfigLoader),
	% TODO Access the configuration directly in
	% the app modules to allows hot configuration reloading.
	% Trigger a first configuration load here to ensure the config loader works.
	% TODO Return a bad_configuration error in case of bad match, without making the code ugly?
	#{
		smtp_gateway := SmtpGateway,
		users := Users,
		devices := Devices,
		forwarders := Forwarders
	} = ConfigLoader:load_config(),
	lager:debug("Users are ~p", [Users]),
	% TODO Start as an unsupervised process?
	% How to start the gen_smtp_server in geo_gateway_sup as a supervised
	% process?
	% https://learnyousomeerlang.com/supervisors
	% https://marcelog.github.io/articles/erlang_special_processes_tutorial_handling_system_messages.html
	% Use a monitor at least?
	% https://marcelog.github.io/articles/erlang_link_vs_monitor_difference.html
	% TODO Allows to specify the server name.
	{ok,_Pid} = gen_smtp_server:start(geo_gateway_smtp_server1, geo_gateway_smtp_server, [
		{port, maps:get(port, SmtpGateway, 25)},
		{sessionoptions,
			[{callbackoptions,
				[
					{auth, true},
					{dumps_incoming, maps:get(dumps_incoming, SmtpGateway, false)},
					{dumps_directory, maps:get(dumps_directory, SmtpGateway, "dumps/")},
					{users, Users},
					{devices, Devices},
					{forwarders, Forwarders}
				]
			}]
		}
	]),
	geo_gateway_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
	gen_smtp_server:stop(geo_gateway_smtp_server1),
	ok.

%%====================================================================
%% Internal functions
%%====================================================================
