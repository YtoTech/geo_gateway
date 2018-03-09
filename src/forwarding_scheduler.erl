%% @doc A forwarding scheduler.
%%
%% Responsible for scheduling forwarding task in worker processes and handling
%% their failures.
%%
%% Manage forwarding workload, with retry on forwarder failure, load management.

-module(forwarding_scheduler).
-author('yoan@ytotech.com').

% Naive implementation. Should be in its own module.
% TODO This could be a gen_server? --> with special handler for trapping exits.

%% Scheduler API.
-export([
	start_scheduler/0, schedule/1
]).
%% For tests.
-export([
	reschedule_compute_delay/1
]).

% TODO Make MAX_RETRIES configurable.
-define(MAX_RETRIES, 11).
-define(RETRY_DELAY, 500).
-define(RETRY_MAX_BACKOFF_FACTOR, 240).

-record(
	state,
	{
		to_schedule = [] :: list(),
		running = #{} :: map(),
		is_shuttingdown = false :: boolean()
	}
).

%%====================================================================
%% API functions.
%%====================================================================

start_scheduler() ->
	Pid = spawn_link(fun() -> init_scheduler() end),
	register(gateway_forwarding_scheduler, Pid),
	{ok, Pid}.


init_scheduler() ->
	process_flag(trap_exit, true),
	scheduler(#state{}).

schedule(ForwarderDescriptor) ->
	% Get a response to ensure the scheduler is alive.
	Ref = make_ref(),
	gateway_forwarding_scheduler ! {to_schedule, self(), Ref, ForwarderDescriptor},
	receive
		{ok, Ref} -> ok;
		{error, Reason} -> {error, Reason}
	after 500
		-> {error, schedule_timeout}
	end.

%%====================================================================
%% Gateway Forwarding Scheduler
%%====================================================================

scheduler(State = #state{to_schedule=[ToSchedule|Others], running=Running}) ->
	% TODO We may support worker pooling strategy to control how many task of a kind
	% (forwarding module) are spawned.
	% For now, this launch all these at the same time.
	% Algo may be:
	% - take from record worker pool, forwarding to run;
	% - ff N worker processes available, and M task to run launch min(N,M)
	% forwarding processes.
	% May use https://github.com/devinus/poolboy or https://github.com/inaka/worker_pool
	scheduler(State#state{
		to_schedule=Others,
		running=maps:put(
			launch_worker(maps:get(forwarder_desc, ToSchedule)),
			ToSchedule,
			Running
		)
	});
scheduler(#state{is_shuttingdown=true, to_schedule=[], running=Running}) when Running =:= #{} ->
	lager:info("Shutting down ok");
scheduler(State = #state{is_shuttingdown=IsShuttingdown, to_schedule=ToSchedule, running=Running}) ->
	% Here receive events and recurse again.
	receive
		{to_schedule, From, Ref, ForwarderDescriptor} ->
			case IsShuttingdown of
				false ->
					From ! {ok, Ref},
					scheduler(State#state{to_schedule=[#{ forwarder_desc => ForwarderDescriptor, retries => 0 }|ToSchedule]});
				true ->
					From ! {error, is_shuttingdown},
					scheduler(State)
			end;
		{to_reschedule, Pid, ToReschedule} when Pid =:= self() ->
			lager:info("Reschedule timeout triggered for ~s", [nested:get([forwarder_desc, module], ToReschedule)]),
			scheduler(State#state{to_schedule=[ToReschedule|ToSchedule]});
		{'EXIT', Pid, normal} ->
			scheduler(State#state{running=maps:remove(Pid, Running)});
		shutdown ->
			% TODO This is not triggered.
			lager:info("Shutting down"),
			scheduler(State#state{is_shuttingdown=true});
		{'EXIT', Pid, shutdown} ->
			% Terminate: wait all forwarding running finish.
			% Continue until running is empty.
			% TODO Should refuse to schedule after shutting down.
			% --> schedule/1 must then return an error shutting_down.
			lager:info("~p is shutting down too", [Pid]),
			scheduler(State#state{is_shuttingdown=true});
		{'EXIT', Pid, Reason} ->
			case maps:is_key(Pid, Running) of
				false ->
					lager:warning("Received EXIT from unknown worker: ~p. Reader: ~p", [Pid, Reason]),
					ok;
				_ -> ok
			end,
			lager:debug("Trapped from ~p: ~p", [Pid, Reason]),
			% Keep track of the number of times the process has been rescheduled.
			% After N tries, just emit a warning and give up.
			{FailedTask, RunningUpdated} = maps:take(Pid, Running),
			case FailedTask of
				#{ retries := Retries } when Retries > ?MAX_RETRIES ->
					% TODO Emit a signal and make it suscrivable?
					% (to store the payload in a special file/facility)
					lager:error("Abort payload forwarding after ~p tries. Task: ~p. Reason: ~p", [Retries, FailedTask, Reason]),
					scheduler(State#state{running=RunningUpdated});
				_ ->
					reschedule(State, FailedTask, RunningUpdated)
			end;
		{Message} ->
			lager:error("Received unknown message: ~p", [Message]),
			scheduler(State)
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

reschedule(State, FailedTask, RunningUpdated) ->
	% TODO What about reschedule tasks during shut down?
	ToReschedule = maps:update(retries, maps:get(retries, FailedTask) + 1, FailedTask),
	lager:debug("Reschedule ~p", [ToReschedule]),
	Delay = reschedule_compute_delay(ToReschedule),
	lager:info(
		"Forwarding task ~s reschedule in ~p s after ~p tries", [
			nested:get([forwarder_desc, module], ToReschedule),
			Delay/1000,
			maps:get(retries, ToReschedule)
	]),
	% TODO Keep track of reschedule task timers in state?
	{ok, _TRef} = timer:send_after(Delay, {to_reschedule, self(), ToReschedule}),
	scheduler(State#state{running=RunningUpdated}).

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
	backoff_rec(maps:get(retries, ToReschedule), 1) * ?RETRY_DELAY.

backoff_rec(1, D) ->
	D;
backoff_rec(N, D) ->
	backoff_rec(N-1, backoff:increment(D, ?RETRY_MAX_BACKOFF_FACTOR)).
