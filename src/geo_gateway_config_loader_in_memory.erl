%%%-------------------------------------------------------------------
%% @doc Store gateway configuration in a process dictionary.
%% @end
%%%-------------------------------------------------------------------

-module(geo_gateway_config_loader_in_memory).

-behaviour(geo_gateway_config_loader).
-behaviour(gen_server).

%% API functions.
-export([
	start_link/0, stop/0, load_config/0, set_config/1
]).
%% gen_server callbacks.
-export([
	init/1, handle_call/3, handle_cast/2
]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
	gen_server:start_link({local, geo_gateway_config_loader_in_memory}, ?MODULE, [], []).

stop() ->
	gen_server:stop(geo_gateway_config_loader_in_memory).

-spec load_config() -> #{devices => map(), forwarders => map(), smtp_gateway => map(), users => map()}.
load_config() ->
	gen_server:call(geo_gateway_config_loader_in_memory, load_config).

-spec set_config(Config :: map()) -> ok.
set_config(Config) ->
	gen_server:call(geo_gateway_config_loader_in_memory, {set_config, Config}).

%%====================================================================
%% Internal functions
%%====================================================================

init([]) ->
	{ok, undefined}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_call(load_config, _From, State) ->
	{reply, State, State};

handle_call({set_config, Config}, _From, _State) ->
	{reply, ok, Config};

handle_call(_Request, _From, State) ->
	{noreply, State}.
