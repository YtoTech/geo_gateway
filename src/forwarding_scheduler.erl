%% @doc A forwarding scheduler.
%%
%% Responsible for scheduling forwarding task in worker processes and handling
%% their failures.
%%
%% Manage forwarding workload, with retry on forwarder failure, load management.
%%
%% We reimplement in a great part the https://hexdocs.pm/gen_retry/GenRetry.html
%% bahviour for our forwarding tasks.

-module(forwarding_scheduler).
-author('yoan@ytotech.com').

-behaviour(gen_server).

%% Scheduler API.
-export([
	start_link/0, stop/0, schedule/1
]).
%% gen_server callbacks.
-export([
	init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2
]).
%% For tests.
-export([
	reschedule_compute_delay/1
]).

% TODO Make MAX_RETRIES configurable.
-define(MAX_RETRIES, 11).
-define(RETRY_DELAY, 500).
-define(RETRY_MAX_BACKOFF_DELAY, 120000).

-record(
	state,
	{
		to_schedule = [] :: list(),
		running = #{} :: map(),
		is_shuttingdown = false :: boolean(),
		shutdown_ref = nil :: tuple(),
		rescheduled = #{} :: map()
	}
).

%%====================================================================
%% API functions.
%%====================================================================

start_link() ->
	gen_server:start_link({local, gateway_forwarding_scheduler}, ?MODULE, [], []).

stop() ->
	% Here we ask the scheduler to terminate properly using the stop request.
	% It lets the gen_server continue to work, while our forwarding server knows
	% it must shot down asap.
	% In practice the forwarding server stop accepting new tasks and try to run
	% any remaining tasks successfully before actually stoppping.
	gen_server:call(gateway_forwarding_scheduler, stop, infinity).

init([]) ->
	process_flag(trap_exit, true),
	{ok, #state{}}.

schedule(ForwarderDescriptor) ->
	% Get a response to ensure the scheduler is alive.
	lager:debug("Scheduling request ~p", [ForwarderDescriptor]),
	gen_server:call(gateway_forwarding_scheduler, {to_schedule, ForwarderDescriptor}).

%%====================================================================
%% Gateway Forwarding Scheduler
%%====================================================================

handle_cast(schedule, State) ->
	% Scheduling round.
	scheduler(State);
handle_cast(_Message, State) ->
    {noreply, State}.

handle_call({to_schedule, ForwarderDescriptor}, _From, State = #state{is_shuttingdown=false, to_schedule=ToSchedule}) ->
	gen_server:cast(self(), schedule),
	{reply, ok, State#state{to_schedule=[#{ forwarder_desc => ForwarderDescriptor, retries => 0 }|ToSchedule]}};
handle_call({to_schedule, _ForwarderDescriptor}, _From, State = #state{is_shuttingdown=true}) ->
	{reply, {error, is_shuttingdown}, State};
handle_call(stop, From, State) ->
	% Let the server knows it must clean up and answer to the stop request.
	lager:info("Stopping the scheduler"),
	gen_server:cast(self(), schedule),
	{noreply, State#state{is_shuttingdown=true, shutdown_ref=From}};
handle_call(_Message, _From, State) ->
	{reply, {error, invalid_request}, State}.

scheduler(State = #state{to_schedule=[ToSchedule|Others], running=Running}) ->
	% TODO We may support worker pooling strategy to control how many task of a kind
	% (forwarding module) are spawned.
	% For now, this launch all these at the same time.
	% Algo may be:
	% - take from record worker pool, forwarding to run;
	% - ff N worker processes available, and M task to run launch min(N,M)
	% forwarding processes.
	% May use https://github.com/devinus/poolboy or https://github.com/inaka/worker_pool
	% TODO Pooling may be managed in the forwarding_server? This is another concern
	% as the supervision and relaunch strategy of worker task.
	scheduler(State#state{
		to_schedule=Others,
		running=maps:put(
			launch_worker(maps:get(forwarder_desc, ToSchedule)),
			ToSchedule,
			Running
		)
	});
scheduler(State = #state{is_shuttingdown=true, shutdown_ref=From, to_schedule=[], running=Running, rescheduled=Rescheduled}) when Running =:= #{}, Rescheduled =:= #{} ->
	lager:info("No more tasks to run: ready to shut down"),
	gen_server:reply(From, ok),
	{stop, normal, State};
scheduler(State = #state{to_schedule=[]}) ->
	{noreply, State}.

handle_info({'EXIT', Pid, normal}, State = #state{running=Running}) ->
	gen_server:cast(self(), schedule),
	{noreply, State#state{running=maps:remove(Pid, Running)}};
handle_info({'EXIT', Pid, Reason}, State = #state{running=Running}) ->
	case maps:is_key(Pid, Running) of
		false ->
			lager:warning("Received EXIT from unknown worker: ~p. Reader: ~p", [Pid, Reason]),
			{noreply, State};
		_ ->
			on_worker_error(Pid, Reason, State)
	end;
handle_info({to_reschedule, Pid, Ref, ToReschedule}, State = #state{to_schedule=ToSchedule, rescheduled=Rescheduled}) when Pid =:= self() ->
	lager:info("Reschedule timeout triggered for ~s", [nested:get([forwarder_desc, module], ToReschedule)]),
	gen_server:cast(self(), schedule),
	{noreply, State#state{to_schedule=[ToReschedule|ToSchedule], rescheduled=maps:remove(Ref, Rescheduled)}};
handle_info(Message, State) ->
	lager:error("Received unknown message: ~p", [Message]),
	{noreply, State}.

on_worker_error(Pid, Reason, State = #state{running=Running}) ->
	lager:debug("Trapped from ~p: ~p", [Pid, Reason]),
	% Keep track of the number of times the process has been rescheduled.
	% After N tries, just emit a warning and give up.
	{FailedTask, RunningUpdated} = maps:take(Pid, Running),
	gen_server:cast(self(), schedule),
	case FailedTask of
		#{ retries := Retries } when Retries > ?MAX_RETRIES ->
			% TODO Emit a signal and make it suscrivable?
			% (to store the payload in a special file/facility)
			lager:error("Abort payload forwarding after ~p tries. Task: ~p. Reason: ~p", [Retries, FailedTask, Reason]),
			{noreply, State#state{running=RunningUpdated}};
		_ ->
			reschedule(State, FailedTask, RunningUpdated)
	end.

launch_worker(ForwarderDescriptor) ->
	spawn_link(
		fun() ->
			ReturnOk = maps:get(return_ok, ForwarderDescriptor),
			ReturnOk = erlang:apply(
				maps:get(module, ForwarderDescriptor),
				maps:get(function, ForwarderDescriptor),
				maps:get(args, ForwarderDescriptor)
			)
		end
	).

reschedule(State = #state{rescheduled=Rescheduled}, FailedTask, RunningUpdated) ->
	ToReschedule = maps:update(retries, maps:get(retries, FailedTask) + 1, FailedTask),
	lager:debug("Reschedule ~p", [ToReschedule]),
	Delay = reschedule_compute_delay(ToReschedule),
	lager:info(
		"Forwarding task ~s reschedule in ~p s after ~p tries", [
			nested:get([forwarder_desc, module], ToReschedule),
			Delay/1000,
			maps:get(retries, ToReschedule)
	]),
	Ref = make_ref(),
	{ok, TRef} = timer:send_after(Delay, {to_reschedule, self(), Ref, ToReschedule}),
	RescheduledUpdated = maps:put(Ref, #{ timer => TRef, task => ToReschedule }, Rescheduled),
	{noreply, State#state{running=RunningUpdated, rescheduled=RescheduledUpdated}}.

terminate(normal, _State) ->
	lager:info("Shutting down ok"),
    ok.

%%====================================================================
%% Private util functions.
%%====================================================================

reschedule_compute_delay(ToReschedule) ->
	% Reschedule after a delay to avoid getting in a spamming failure-loop
	% just after a transient error occured. Use a back-off exponential delay
	% algorithm similar to TCP-one.
	% TODO Also make the delay a bit random (borned random) to avoid
	% triggering a bunch of similar task on the same forwarding service
	% at the same time (for e.g. when we received a batch of manies and X % timeout
	% at the same time on their first transmission).
	backoff_delay(maps:get(retries, ToReschedule)).

backoff_delay(1) ->
	?RETRY_DELAY;
backoff_delay(FailCount) ->
	% TODO Use rand_increment
	backoff:increment(?RETRY_DELAY bsl (FailCount - 2), ?RETRY_MAX_BACKOFF_DELAY).
