%% @doc Supervise the forwarding scheduler.
%%
%% Mainly forward to him a stop event so it has a chance to terminate properly.

-module(forwarding_scheduler_sup).
-author('yoan@ytotech.com').

-behaviour(gen_server).

%% Scheduler supervisor API.
-export([
	start_link/0, stop/0
]).
%% gen_server callbacks.
-export([
	init/1, handle_call/3, handle_cast/2, terminate/2
]).

%%====================================================================
%% API functions.
%%====================================================================

start_link() ->
	forwarding_scheduler:start_link(),
	gen_server:start_link({local, gateway_forwarding_scheduler_sup}, ?MODULE, [], []).

stop() ->
	gen_server:stop(forwarding_scheduler_sup).

init([]) ->
	process_flag(trap_exit, true),
	{ok, undef}.

handle_cast(_Message, State) ->
    {noreply, State}.

handle_call(_Message, _From, State) ->
	{reply, {error, invalid_request}, State}.

terminate(shutdown, _State) ->
	forwarding_scheduler:stop();
terminate(normal, _State) ->
    ok.
