%%%-------------------------------------------------------------------
%% @doc geo_gateway top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(geo_gateway_sup).

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
	% TODO What gives this ?SERVER const?
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
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
	% TODO Use the new supervisor spec using a Map to be more readable.
	% http://erlang.org/doc/man/supervisor.html
	% http://erlang.org/doc/design_principles/sup_princ.html
	SmtpServer = {
		geo_gateway_smtp_server,
		{gen_smtp_server, start_link, [geo_gateway_smtp_server, [[
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
		[geo_gateway_smtp_server]
	},
	ForwardingServer = {
		geo_gateway_forwarding_router,
		{geo_gateway_forwarding_router, start_link, []},
		permanent,
		10000,
		worker,
		[geo_gateway_forwarding_router]
	},
	% Here we want the sceduler with a timeout greater than the geo_gateway_forwarding_router.
	ForwardingScheduler = {
		geo_gateway_forwarding_scheduler,
		{geo_gateway_forwarding_scheduler_sup, start_link, []},
		permanent,
		240000,
		worker,
		[geo_gateway_forwarding_scheduler_sup, geo_gateway_forwarding_scheduler]
	},
	Children = [SmtpServer, ForwardingServer, ForwardingScheduler],
	RestartStrategy = {one_for_one, 2, 5},
	{ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
