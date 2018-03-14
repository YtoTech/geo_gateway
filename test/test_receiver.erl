%%%-------------------------------------------------------------------
%% @doc A payload receiver used to test forwarding behaviour.
%% @end
%%%-------------------------------------------------------------------

-module(test_receiver).

-behaviour(geo_gateway_module_payload_receiver).
-behaviour(gen_server).

%% API functions.
-export([
	start_link/0, stop/0, on_payload/4, get_received_payloads/0
]).
%% gen_server callbacks.
-export([
	init/1, handle_call/3, handle_cast/2
]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
	gen_server:start_link({local, test_receiver}, ?MODULE, [], []).

stop() ->
	gen_server:stop(test_receiver).

-spec on_payload(Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> ok.
on_payload(Payload, User, Device, Forwarder) ->
	gen_server:call(test_receiver, {on_payload, Payload, User, Device, Forwarder}).

-spec get_received_payloads() -> list().
get_received_payloads() ->
	gen_server:call(test_receiver, get_received_payloads).

%%====================================================================
%% Internal functions
%%====================================================================

init([]) ->
	{ok, []}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_call({on_payload, Payload, _User, _Device, _Forwarder}, _From, State) ->
	{reply, ok, State ++ [Payload]};

handle_call(get_received_payloads, _From, State) ->
	{reply, State, State};

handle_call(_Request, _From, State) ->
	{noreply, State}.
