%% @doc Describing the geo_forwader behaviour, responsible forwarding parsed
%% sensor payloads. Should return immediately and handle forwarding in
%% dedicated processes.
%%
%% TODO Implement as a gen_server? Let the client decide?
-module(geo_forwarder).
-author('yoan@ytotech.com').

-callback forward(Reference :: binary(), Payload :: list(), User :: map(), Device :: map(), Forwarders :: list()) -> 'ok'.
