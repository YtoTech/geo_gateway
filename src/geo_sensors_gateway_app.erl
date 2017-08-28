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
	% TODO Is that to start in the supervision tree?
	% Load authorized users from a json map.
	% TODO Allow to load with no-users.
	% TODO Create a provider for that and allows hot-reloading.
	% TODO Allows to override configuration file path.
	% The users are indexed by username, because we encounter
	% first the AUTH in the process of receiving a mail.
	ConfigurationFileContent = case file:read_file('configuration.json') of
		{ok, CFC} ->
			CFC;
		_ ->
			io:format("Failed to open configuration.json.~nYou can create a
			configuration using example configuration.json.sample.~n"),
			erlang:error(no_configuration)
	end,
	ConfigurationAsJson = jiffy:decode(
		ConfigurationFileContent, [return_maps, use_nil]
	),
	% Parse and reformat configuration.
	% TODO Make it recursive?
	MapKeyToAtom = fun(Key, Value, NewMap) ->
		maps:put(
			binary_to_atom(Key, unicode),
			Value,
			NewMap
		)
	end,
	Users = maps:map(
		fun(_Username, User) ->
			% TODO Handle non-authenticated mode?
			% We make here the assumption that we have one email handled
			% by each username. (An username could allow to handle many
			% mails).
			% TODO Do we really care and want to filter by emails?
			% When the auth is right, we can assume we are fine.
			#{
				password => maps:get(<<"password">>, User),
				email => maps:get(<<"email">>, User),
				device => maps:get(<<"device">>, User),
				forwarders => maps:get(<<"forwarders">>, User, []),
				dumps_raw => maps:get(<<"dumps_raw">>, User, false)
			}
		end,
		maps:get(<<"users">>, ConfigurationAsJson)
	),
	Users = maps:map(
		fun(_Username, User) ->
			% TODO Handle non-authenticated mode?
			% We make here the assumption that we have one email handled
			% by each username. (An username could allow to handle many
			% mails).
			% TODO Do we really care and want to filter by emails?
			% When the auth is right, we can assume we are fine.
			#{
				password => maps:get(<<"password">>, User),
				email => maps:get(<<"email">>, User),
				device => maps:get(<<"device">>, User),
				forwarders => maps:get(<<"forwarders">>, User, []),
				dumps_raw => maps:get(<<"dumps_raw">>, User, false)
			}
		end,
		maps:get(<<"users">>, ConfigurationAsJson, {})
	),
	io:format("Users ~p ~n", [Users]),
	Devices = maps:map(
		fun(_DeviceId, Device) ->
			#{
				manufacturer => maps:get(<<"manufacturer">>, Device),
				range => maps:get(<<"range">>, Device),
				model => maps:get(<<"model">>, Device),
				parameters => maps:fold(MapKeyToAtom, #{}, maps:get(<<"parameters">>, Device, []))
			}
		end,
		maps:get(<<"devices">>, ConfigurationAsJson, {})
	),
	io:format("Devices ~p ~n", [Devices]),
	Forwarders = maps:map(
		fun(_ForwarderId, Forwarder) ->
			#{
				module => maps:get(<<"module">>, Forwarder),
				parameters => maps:fold(MapKeyToAtom, #{}, maps:get(<<"parameters">>, Forwarder, []))
			}
		end,
		maps:get(<<"forwarders">>, ConfigurationAsJson, {})
	),
	io:format("Forwarders ~p ~n", [Forwarders]),
	{ok,_} = gen_smtp_server:start(smtp_server, [[
		% TODO Allows configuration of port. Default to 2525.
		{port, 2525},
		{sessionoptions,
			[{callbackoptions,
				[
					{auth, true},
					{dump, true},
					{users, Users},
					{devices, Devices},
					{forwarders, Forwarders}
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
