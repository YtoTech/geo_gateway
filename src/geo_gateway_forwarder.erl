%% @doc Describing the geo_forwader behaviour, responsible forwarding parsed
%% sensor payloads.
%%
%% The forwarding task is launched in a dedicated process by the geo_gateway_forwarding_scheduler..
%%
%% Forward all payload received in one message by the gateway from a sensor.
%% To forward payload one by one, use geo_gateway_forwarder_one.
-module(geo_gateway_forwarder).
-author('yoan@ytotech.com').

-callback forward(Ref :: binary(), Payloads :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
