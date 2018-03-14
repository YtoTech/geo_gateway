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

-spec forward(Reference :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> ok.
forward(Reference, Payload, User, Device, Forwarders) ->
	gen_server:call(geo_gateway_forwarding_router, {forward, {Reference, Payload, User, Device, Forwarders}}).

%%====================================================================
%% Internal functions
%%====================================================================

init([]) ->
	{ok, undefined}.

handle_cast({do_forward, {Reference, Payloads, User, Device, Forwarders}}, State) ->
	do_forward(Reference, Payloads, User, Device, Forwarders),
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_call({forward, {Reference, Payloads, User, Device, Forwarders}}, _From, State) ->
	% Here generate an async cast to itself to actually forward.
	{
		reply,
		gen_server:cast(geo_gateway_forwarding_router, {do_forward, {Reference, Payloads, User, Device, Forwarders}}),
		State
	};

handle_call(_Request, _From, State) ->
	{noreply, State}.

-spec do_forward(Reference :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
do_forward(Reference, Payloads, User, Device, Forwarders) ->
	% Get the forwarders from user config and transfer the payload to each of
	% them.
	lists:foreach(
		fun(ForwarderId) ->
			case maps:find(ForwarderId, Forwarders) of
				{ok, Forwarder} ->
					% We may use gproc:send for notifying the registered forwarders.
					% That may be totally overkill. https://github.com/uwiger/gproc#use-case-pubsub-patterns
					lager:debug("Forwarder ~p", [Forwarder]),
					Module = binary_to_atom(maps:get(module, Forwarder), unicode),
					lager:info("Forward with module ~s", [Module]),
					case code:ensure_loaded(Module) of
						{module, Module} ->
							% TODO Handle error?
							% Add to forwarding to run and launch a schedule/0 pass.
							ok = case maps:get(one_by_one, Forwarder, false) of
								true ->
									lists:foreach(
										fun (Payload) ->
											geo_gateway_forwarding_scheduler:schedule(#{
												module => Module,
												function => forward_one,
												args => [Reference, Payload, User, Device, Forwarder],
												return_ok => ok
											})
										end,
										Payloads
									);
								_ ->
									geo_gateway_forwarding_scheduler:schedule(#{
										module => Module,
										function => forward,
										args => [Reference, Payloads, User, Device, Forwarder],
										return_ok => ok
									})
							end;
						{error, _Reason} ->
							lager:error("No module ~p for forwarder ~p: ignore", [Module, ForwarderId])
					end;
				_ ->
					lager:warning("No forwarder ~p: ignore", [ForwarderId])
			end
		end,
		maps:get(forwarders, User)
	).
