%% @doc A simple example callback module for `forwarder' that also serves as
%% documentation for the required callback API.

-module(geo_gateway_forwarder_module).
-author('yoan@ytotech.com').

-behavior(geo_gateway_forwarder).
-behavior(geo_gateway_forwarder_one).

%% API
-export([forward/5, forward_one/5]).

-spec forward(Reference :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward(_Reference, Payloads, User, Device, Forwarder) ->
	case 'forward?'(maps:get(parameters, Forwarder)) of
		true ->
			ReceiverModule = binary_to_atom(nested:get([parameters, target_module], Forwarder), unicode),
			{module, _} = code:ensure_loaded(ReceiverModule),
			ReceiverModule:on_payload(Payloads, User, Device, Forwarder);
		_ ->
			{error, forward_failed}
	end.

-spec forward_one(Reference :: binary(), Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> 'ok'.
forward_one(_Reference, Payload, User, Device, Forwarder) ->
	case 'forward?'(maps:get(parameters, Forwarder)) of
		true ->
			ReceiverModule = binary_to_atom(nested:get([parameters, target_module], Forwarder), unicode),
			{module, _} = code:ensure_loaded(ReceiverModule),
			ReceiverModule:on_payload(Payload, User, Device, Forwarder);
		_ ->
			{error, forward_failed}
	end.

'forward?'(#{drop_strategy := random, drop_rate := DropRate}) ->
	RandomFloat = rand:uniform(),
	if
		RandomFloat < DropRate -> false;
		true -> true
	end;

'forward?'(_) ->
	true.
