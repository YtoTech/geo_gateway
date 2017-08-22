%%%-------------------------------------------------------------------
%% @doc geo_sensors_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(geo_sensors_gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

% -export([ensure_all_started/1]).
%
%
% -spec ensure_all_started(atom()) -> ok | {error, term()}.
% ensure_all_started(App) ->
%     start_ok(App, application:start(App, permanent)).
%
% -spec start_ok(atom(), ok | {error, term()}) -> ok | {error, {term(), atom()}}.
% start_ok(_App, ok) -> ok;
% start_ok(_App, {error, {already_started, _App}}) -> ok;
% start_ok(App, {error, {not_started, Dep}}) ->
%     ok = ensure_all_started(Dep),
%     ensure_all_started(App);
% start_ok(App, {error, Reason}) ->
%     {error, {Reason, App}}.

start(_StartType, _StartArgs) ->
	% TODO Use http://erlang.org/doc/apps/kernel/application.html#ensure_all_started-1
    {ok,_} = gen_smtp_server:start(smtp_server, [[
        % TODO Allows configuration of port. Default to 2525.
        {port, 2525},
        {sessionoptions,
            [{callbackoptions,
                [
                    {auth, true},
                    {parse, true},
                    {dump, true}
                ]
            }]
        }
        ]]),
    % TODO Use the supervisor for hot-reloading?
    geo_sensors_gateway_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
