%% @doc A forwarding server, implementing the behaviour geo_forwarder.
%%
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in (a) dedicated process. When this forwarding server has
%% accepted a set of payloads, it is then responsible for their proper delivery,
%% their loss, etc.
%%
%% Manage forwarding strategy, with retry on forwarder failure, optional persistence
%% before delivery.

-module(forwarding_server).
-author('yoan@ytotech.com').

-behaviour(geo_forwarder).
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
	start_scheduler(),
	gen_server:start_link({local, gateway_forwarding_server}, ?MODULE, [], []).

stop() ->
	gen_server:stop(gateway_forwarding_server).

-spec forward(Reference :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> ok.
forward(Reference, Payload, User, Device, Forwarders) ->
	gen_server:call(gateway_forwarding_server, {forward, {Reference, Payload, User, Device, Forwarders}}).

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
		gen_server:cast(gateway_forwarding_server, {do_forward, {Reference, Payload, User, Device, Forwarders}}),
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
					% io:format("Forwarder ~p~n", [Forwarder]),
					Module = binary_to_atom(maps:get(module, Forwarder), unicode),
					% io:format("Forwarder ~s~n", [Module]),
					case code:ensure_loaded(Module) of
						{module, Module} ->
							% TODO Handle error?
							% Add to forwarding to run and launch a schedule/0 pass.
							ok = schedule(#{
								module => Module,
								function => forward_one,
								args => [Reference, Payload, User, Device, Forwarder]
							});
							% ok = Module:forward_one(
							% 	Reference, Payload, User, Device, Forwarder
							% );
						{error, _Reason} ->
							% TODO Or crash?
							io:format("No module ~p for forwarder ~p: ignore~n", [Module, ForwarderId])
					end;
				_ ->
					io:format("No forwarder ~p: ignore~n", [ForwarderId])
			end
		end,
		maps:get(forwarders, User)
	).

%%====================================================================
%% Gateway Forwarding Scheduler
%%====================================================================
% Naive implementation. Should be in its own module.
% This could be a gen_server? --> with special handler for trapping exits.

-record(
	state,
	{
		to_schedule = [] :: list(),
		running = #{} :: map(),
		is_shuttingdown = false :: boolean()
	}
).

start_scheduler() ->
	Pid = spawn_link(fun() -> init_scheduler() end),
	register(gateway_forwarding_scheduler, Pid).

init_scheduler() ->
	% TODO Use monitors, not links, as trapping exit signals will give us any
	% error signal (including from the forwarding server: bidirectionnal).
	process_flag(trap_exit, true),
	scheduler(#state{}).

schedule(Forwarding) ->
	{to_schedule, _} = gateway_forwarding_scheduler ! {to_schedule, Forwarding},
	ok.

scheduler(State = #state{is_shuttingdown=false, to_schedule=[ToSchedule|Others], running=Running}) ->
	% TODO (Add worker pooling) Take from record: worker pool, forwarding to run.
	% If N worker processes available, and M task to run,
	% launch min(N,M) forwarding processes. (linking to them)
	scheduler(State#state{
		to_schedule=Others,
		running=maps:put(
			launch_worker(ToSchedule),
			ToSchedule,
			Running
		)
	});
scheduler(State = #state{is_shuttingdown=true, running=#{}}) ->
	io:format("Shutting down ok~n"),
	exit(normal);
scheduler(State = #state{to_schedule=ToSchedule, running=Running}) ->
	% Here receive and recurse again.
	receive
		{to_schedule, Forwarding} ->
			scheduler(State#state{to_schedule=[Forwarding|ToSchedule]});
		{'EXIT', Pid, normal} ->
			scheduler(State#state{running=maps:remove(Pid, Running)});
		{'EXIT', _, shutdown} ->
			% Continue until running is empty.
			% TODO Should refuse to schedule after shutting down.
			% --> schedule/1 must then return an error shutting_down.
			io:format("Shutting down~n"),
			scheduler(State#state{is_shuttingdown=true});
		{'EXIT', Pid, Reason} ->
			io:format("Trapped from ~p: ~p~n", [Pid, Reason]),
			% TODO Keep track of the number of times the process has been rescheduled.
			% After N tries, just emit a warning and give up.
			{Fowarding, RunningUpdated} = maps:take(Pid, Running),
			io:format("Reschedule ~p~n", [Fowarding]),
			scheduler(State#state{to_schedule=[Fowarding|ToSchedule], running=RunningUpdated})
	end.

launch_worker(Forwarding) ->
	spawn_link(
		maps:get(module, Forwarding),
		maps:get(function, Forwarding),
		maps:get(args, Forwarding)
	).

% Trap EXIT from forwarding processes.
% If normal, remove from forwarding running: ok.
% If error, remove from forwarding running and put back on top or forwarding to run.
% Launch schedule/0.

% Terminate: wait all forwarding running finish.
