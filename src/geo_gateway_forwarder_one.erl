%% @doc Describing the geo_forwader behaviour, responsible forwarding parsed
%% sensor payloads.
%%
%% The forwarding task is launched in a dedicated process by the geo_gateway_forwarding_scheduler..
%%
%% Forward payloads one by one, even if the gateway received a batch of payload
%% from a sensor.
%% To forward payloads as batch, use geo_gateway_forwarder.
-module(geo_gateway_forwarder_one).
-author('yoan@ytotech.com').

-callback forward_one(Ref :: binary(), Payload :: map(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
