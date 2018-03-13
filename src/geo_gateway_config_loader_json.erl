%%%-------------------------------------------------------------------
%% @doc Load gateway configuration from a json file.
%% @end
%%%-------------------------------------------------------------------

-module(geo_gateway_config_loader_json).

-behaviour(geo_gateway_config_loader).

%% API functions.
-export([
	load_config/0
]).

%%====================================================================
%% API
%%====================================================================

-spec load_config() -> #{devices => map(), forwarders => map(), smtp_gateway => map(), users => map()}.
load_config() ->
	% TODO Use get_env as the first mechanism to pass configuration.
	% TODO Use http://erlang.org/doc/apps/kernel/application.html#ensure_all_started-1
	% TODO Is that to start in the supervision tree?
	% Load authorized users from a json map.
	% TODO Allow to load with no-users.
	% TODO Create a provider for that and allows hot-reloading.
	% TODO Allows to override configuration file path.
	% The users are indexed by username, because we encounter
	% first the AUTH in the process of receiving a mail.
	{ok, ConfigFilePath} = application:get_env(geo_gateway, json_configuration_file),
	lager:info("Using configuration file ~s", [ConfigFilePath]),
	ConfigurationFileContent = case file:read_file(ConfigFilePath) of
		{ok, CFC} ->
			CFC;
		_ ->
			lager:warning("Failed to open \"~s\". You can create a
			configuration from the example configuration.json.sample.", [ConfigFilePath]),
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
				dumps_incoming => maps:get(<<"dumps_incoming">>, User, false)
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
				dumps_incoming => maps:get(<<"dumps_incoming">>, User, false)
			}
		end,
		maps:get(<<"users">>, ConfigurationAsJson, {})
	),
	lager:debug("Users ~p", [Users]),
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
	lager:debug("Devices ~p", [Devices]),
	Forwarders = maps:map(
		fun(_ForwarderId, Forwarder) ->
			#{
				module => maps:get(<<"module">>, Forwarder),
				parameters => maps:fold(MapKeyToAtom, #{}, maps:get(<<"parameters">>, Forwarder, []))
			}
		end,
		maps:get(<<"forwarders">>, ConfigurationAsJson, {})
	),
	lager:debug("Forwarders ~p", [Forwarders]),
	SmtpGateway = maps:fold(MapKeyToAtom, #{}, maps:get(<<"smtp_gateway">>, ConfigurationAsJson, #{})),
	lager:debug("SmtpGateway ~p", [SmtpGateway]),
	#{
		users => Users,
		devices => Devices,
		forwarders => Forwarders,
		smtp_gateway => SmtpGateway
	}.

%%====================================================================
%% Internal functions
%%====================================================================
