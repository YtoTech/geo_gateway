%%%-------------------------------------------------------------------
%% @doc Store gateway configuration in a process dictionary.
%% @end
%%%-------------------------------------------------------------------

-module(gateway_config_loader_process_dict).

-behaviour(gateway_config_loader).

%% API functions.
-export([
	load_config/0, set_config/1
]).

%%====================================================================
%% API
%%====================================================================

% TODO Even if just for testing, it will be proper as a gen_server callback module.
% (than maintening state with get and put on the process).
% https://stackoverflow.com/a/6225749/1956471

-spec load_config() -> #{devices => map(), forwarders => map(), smtp_gateway => map(), users => map()}.
load_config() ->
	get("geo_gateway_config").

-spec set_config(Config :: map()) -> ok.
set_config(Config) ->
	put("geo_gateway_config", Config),
	ok.

%%====================================================================
%% Internal functions
%%====================================================================
