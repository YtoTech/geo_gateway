%% @doc Forwarding router that fan out parsed payloads or batch of payloads to
%% forwarders defined in configuration.
%%
%% Responsible for orchestrating parsed sensor payloads forwarding strategy.
%% Should return immediately and handle forwarding in a dedicated process.
%% When this forwarding server has accepted a set of payloads,
%% it is then responsible for their proper delivery, their loss, etc.
%%
%% Rely on the geo_gateway_forwarding_scheduler to launch and monitor the
%% forwarding workers.

-module(geo_gateway_forwarding_router).
-author('yoan@ytotech.com').

-behaviour(gen_server).

%% API functions.
-export([
	start_link/0, stop/0, forward/5
]).
%% gen_server callbacks.
-export([
	init/1, handle_call/3, handle_cast/2
]).

%%====================================================================
%% API
%%====================================================================

start_link() ->
	gen_server:start_link({local, geo_gateway_forwarding_router}, ?MODULE, [], []).

stop() ->
	gen_server:stop(geo_gateway_forwarding_router).

-spec forward(Ref :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> ok.
forward(Ref, Payload, User, Device, Forwarders) ->
	gen_server:call(geo_gateway_forwarding_router, {forward, {Ref, Payload, User, Device, Forwarders}}).

%%====================================================================
%% Internal functions
%%====================================================================

init([]) ->
	{ok, undefined}.

handle_cast(_Request, State) ->
	{noreply, State}.

handle_call({forward, {Ref, Payloads, User, Device, Forwarders}}, _From, State) ->
	{reply, do_forward(Ref, Payloads, User, Device, Forwarders), State};
handle_call(_Request, _From, State) ->
	{noreply, State}.

-spec do_forward(Ref :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
do_forward(Ref, Payloads, User, Device, Forwarders) ->
	% Get the forwarders from user config and transfer the payload to each of
	% them.
	lists:foreach(
		fun(ForwarderId) ->
			Forwarder = maps:get(ForwarderId, Forwarders),
			lager:debug("Forwarder ~p", [Forwarder]),
			do_forward_forwarder(Ref, Payloads, User, Device, Forwarder)
		end,
		maps:get(forwarders, User)
	).

-spec do_forward_forwarder(Ref :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
do_forward_forwarder(Ref, Payloads, User, Device, Forwarder) ->
	Module = binary_to_atom(maps:get(module, Forwarder), unicode),
	lager:info("Forward with module ~s", [Module]),
	{module, Module} = code:ensure_loaded(Module),
	% Add to forwarding to run and launch a schedule/0 pass.
	ok = case maps:get(one_by_one, Forwarder, false) of
		true ->
			lists:foreach(
				fun (Payload) ->
					geo_gateway_forwarding_scheduler:schedule(#{
						module => Module,
						function => forward_one,
						args => [Ref, Payload, User, Device, Forwarder],
						return_ok => ok
					})
				end,
				Payloads
			);
		_ ->
			geo_gateway_forwarding_scheduler:schedule(#{
				module => Module,
				function => forward,
				args => [Ref, Payloads, User, Device, Forwarder],
				return_ok => ok
			})
	end.
