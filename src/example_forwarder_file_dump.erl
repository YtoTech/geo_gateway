%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in dedicated processes.

-module(example_forwarder_file_dump).
-author('yoan@ytotech.com').
% TODO https://stackoverflow.com/questions/32336854/how-to-create-and-use-a-custom-erlang-behavior
% -behaviour(forwarder).

%% API
-export([forward/5]).

-spec forward(Reference :: binary(), Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward(Reference, Payload, _User, _Device, Forwarder) ->
	% TODO Do forward in another process.
	% Basically add to a queue using Erlang messages.
	% Create one new process per forwarding?
	FilePath = erlang:iolist_to_binary([
		nested:get([parameters, path], Forwarder), <<"/">>, Reference,<<".payload">>
	]),
	Formatted = io_lib:format(
		"~p\n", [Payload]
	),
	io:format("Write to: ~s~n", [FilePath]),
	ok = file:write_file(
		FilePath, Formatted
	),
	ok.
