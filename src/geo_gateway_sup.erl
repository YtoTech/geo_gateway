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
	% TODO Put again the gen_smtp_server in supervision tree.
	% https://erlang.org/doc/man/supervisor.html
	% SmtpServer = #{
	% 	id => geo_gateway_smtp_server,
	% 	start => {gen_smtp_server, start, [geo_gateway_smtp_server, [[
	% 		{port, maps:get(port, SmtpGateway, 25)},
	% 		{sessionoptions,
	% 			[{callbackoptions,
	% 				[
	% 					{auth, true},
	% 					{dumps_incoming, maps:get(dumps_incoming, SmtpGateway, false)},
	% 					{dumps_directory, maps:get(dumps_directory, SmtpGateway, "dumps/")},
	% 					{users, Users},
	% 					{devices, Devices},
	% 					{forwarders, Forwarders}
	% 				]
	% 			}]
	% 		}
	% 	]]]},
	% 	restart => permanent,
	% 	shutdown => 2000,
	% 	type => worker,
	% 	modules => [geo_gateway_smtp_server]
	% },
	ForwardingServer = #{
		id => geo_gateway_forwarding_router,
		start => {geo_gateway_forwarding_router, start_link, []},
		restart => permanent,
		shutdown => 10000,
		type => worker,
		modules => [geo_gateway_forwarding_router]
	},
	% Here we want the sceduler with a timeout greater than the geo_gateway_forwarding_router.
	ForwardingScheduler = #{
		id => geo_gateway_forwarding_scheduler_sup,
		start => {geo_gateway_forwarding_scheduler_sup, start_link, []},
		restart => permanent,
		shutdown => 240000,
		type => worker,
		modules => [geo_gateway_forwarding_scheduler_sup, geo_gateway_forwarding_scheduler]
	},
	Children = [ForwardingServer, ForwardingScheduler],
	RestartStrategy = {one_for_one, 2, 5},
	{ok, { RestartStrategy, Children} }.

%%====================================================================
%% Internal functions
%%====================================================================
