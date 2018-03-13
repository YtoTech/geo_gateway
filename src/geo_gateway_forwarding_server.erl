%% @doc A forwarding server, implementing the behaviour geo_gateway_forwarder.
%%
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in (a) dedicated process. When this forwarding server has
%% accepted a set of payloads, it is then responsible for their proper delivery,
%% their loss, etc.
%%
%% Manage forwarding strategy, with optional persistence before delivery.

-module(geo_gateway_forwarding_server).
-author('yoan@ytotech.com').

-behaviour(geo_gateway_forwarder).
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
	gen_server:start_link({local, geo_gateway_forwarding_server}, ?MODULE, [], []).

stop() ->
	gen_server:stop(geo_gateway_forwarding_server).

-spec forward(Reference :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> ok.
forward(Reference, Payload, User, Device, Forwarders) ->
	gen_server:call(geo_gateway_forwarding_server, {forward, {Reference, Payload, User, Device, Forwarders}}).

%%====================================================================
%% Internal functions
%%====================================================================

init([]) ->
	{ok, undefined}.

handle_cast({do_forward, {Reference, Payload, User, Device, Forwarders}}, State) ->
	% TODO Here manage failure: supervize forwarding processes, trap failures,
	% retries. Or in do_forward for each forwarding? Requires a State? Also timeouts?
	do_forward(Reference, Payload, User, Device, Forwarders),
	{noreply, State};

handle_cast(_Request, State) ->
	{noreply, State}.

handle_call({forward, {Reference, Payload, User, Device, Forwarders}}, _From, State) ->
	% Here generate an async cast to itself to actually forward.
	% TODO Add it to the transmission queue? Add it in state? Or our queue is "message passing"?
	% Simply use map with Key: tuple {Reference, Forwarder}?
	% https://erldocs.com/maint/stdlib/maps.html
	% This would be enough for managing 1000+ forwarding states.
	% (until we want distribution and persistence -> https://erldocs.com/18.0/mnesia/mnesia.html)
	% Put the dict in a record. #forwards
	{
		reply,
		gen_server:cast(geo_gateway_forwarding_server, {do_forward, {Reference, Payload, User, Device, Forwarders}}),
		State
	};

handle_call(_Request, _From, State) ->
	{noreply, State}.

-spec do_forward(Reference :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
do_forward(Reference, Payload, User, Device, Forwarders) ->
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
							lager:debug("Ask for scheduling of ~p", [Reference]),
							ok = geo_gateway_forwarding_scheduler:schedule(#{
								module => Module,
								function => forward_one,
								args => [Reference, Payload, User, Device, Forwarder],
								return_ok => ok
							});
							% ok = Module:forward_one(
							% 	Reference, Payload, User, Device, Forwarder
							% );
						{error, _Reason} ->
							lager:error("No module ~p for forwarder ~p: ignore", [Module, ForwarderId])
					end;
				_ ->
					lager:warning("No forwarder ~p: ignore", [ForwarderId])
			end
		end,
		maps:get(forwarders, User)
	).
