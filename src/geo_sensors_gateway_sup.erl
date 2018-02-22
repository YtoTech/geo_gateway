%%%-------------------------------------------------------------------
%% @doc geo_sensors_gateway top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(geo_sensors_gateway_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	io:format("Init with ~p~n", [application:get_all_env()]),
	% Load application configuration.
	% TODO Access the configuration directly in
	% the app modules.
	% Make geo_sensors_gateway_config a gen_server
	% to be started here. Then it could loads the configuration
	% from files, get_env, HTTP or whatever using a dedicated
	% callback module to be provided.
	#{
		smtp_gateway := SmtpGateway,
		users := Users,
		devices := Devices,
		forwarders := Forwarders
	} = gateway_config_loader_json:load_config(),
	SmtpServer = {
		gen_sensors_gateway,
		{gen_smtp_server, start_link, [smtp_server, [[
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
		]]]},
		permanent,
		2000,
		worker,
		[gen_smtp_server]
	},
	Children = [SmtpServer],
	RestartStrategy = {one_for_one, 0, 1},
	{ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
