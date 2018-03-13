%% @doc Describing the geo_forwader behaviour, responsible forwarding parsed
%% sensor payloads. Should return immediately and handle forwarding in
%% dedicated processes.
%%
-module(geo_gateway_forwarder).
-author('yoan@ytotech.com').

% TODO Allows two forwarder type:
% - forward (all payloads received in a mail);
% - forward_one (one by one).
-callback forward(Reference :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
