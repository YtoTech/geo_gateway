%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.

-module(geo_gateway_forwarder_file_dump).
-author('yoan@ytotech.com').

-behavior(geo_gateway_forwarder).

%% API
-export([forward/5]).

-spec forward(Ref :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward(Ref, Payloads, _User, _Device, Forwarder) ->
	FilePath = erlang:iolist_to_binary([
		nested:get([parameters, path], Forwarder), <<"/">>, Ref,<<".payload">>
	]),
	Formatted = io_lib:format(
		"~p\n", [Payloads]
	),
	lager:debug("Write payload dump to: ~s", [FilePath]),
	ok = file:write_file(
		FilePath, Formatted
	).
