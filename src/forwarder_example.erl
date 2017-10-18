%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in dedicated processes.

-module(forwarder_example).
-author('yoan@ytotech.com').
% TODO https://stackoverflow.com/questions/32336854/how-to-create-and-use-a-custom-erlang-behavior
% -behaviour(forwarder).

%% API
-export([forward/5]).

-spec forward(Reference :: binary(), Payload :: map(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
forward(Reference, Payload, User, Device, Forwarders) ->
	% Get the forwarders from user config and transfer the payload to each of
	% them.
	% TODO (Add it to the transmission queue)
	lists:foreach(
		fun(ForwarderId) ->
			case maps:find(ForwarderId, Forwarders) of
				{ok, Forwarder} ->
					io:format("Forwarder ~p~n", [Forwarder]),
					Module = binary_to_atom(maps:get(module, Forwarder), unicode),
					io:format("Forwarder ~s~n", [Module]),
					case code:ensure_loaded(Module) of
						{module, Module} ->
							% TODO Handle error?
							ok = Module:forward(
								Reference, Payload, User, Device, Forwarder
							);
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
