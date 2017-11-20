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
	#{
		smtp_gateway := SmtpGateway,
		users := Users,
		devices := Devices,
		forwarders := Forwarders
	} = geo_sensors_gateway_config:load_config(),
	% io:format("Config ~p ~p ~p ~p ~n", [SmtpGateway,Users,Devices,Forwarders]),
	% {ok,_} = gen_smtp_server:start_link(smtp_server, [[
	% 	{port, maps:get(port, SmtpGateway, 25)},
	% 	{sessionoptions,
	% 		[{callbackoptions,
	% 			[
	% 				{auth, true},
	% 				{dumps_incoming, maps:get(dumps_incoming, SmtpGateway, false)},
	% 				{dumps_directory, maps:get(dumps_directory, SmtpGateway, "dumps/")},
	% 				{users, Users},
	% 				{devices, Devices},
	% 				{forwarders, Forwarders}
	% 			]
	% 		}]
	% 	}
	% ]]),
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
