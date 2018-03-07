%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.
%% Responsible for forwarding parsed sensor payloads. Should return immediately
%% and handle forwarding in dedicated processes.

-module(example_forwarder_file_dump).
-author('yoan@ytotech.com').

%% API
-export([forward_one/5]).

-spec forward_one(Reference :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward_one(Reference, Payloads, _User, _Device, Forwarder) ->
	% TODO Do forward in another process.
	% Basically add to a queue using Erlang messages.
	% Create one new process per forwarding?
	FilePath = erlang:iolist_to_binary([
		nested:get([parameters, path], Forwarder), <<"/">>, Reference,<<".payload">>
	]),
	Formatted = io_lib:format(
		"~p\n", [Payloads]
	),
	io:format("Write payload dump to: ~s~n", [FilePath]),
	ok = file:write_file(
		FilePath, Formatted
	),
	ok.
