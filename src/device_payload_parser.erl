%% @doc Describing the device_payload_parser behaviour, responsible for parsing sensor
%% payloads and extract all usefull information from them.
%%
%% This data will then be transferred to any endpoint by forwarders.
-module(device_payload_parser).
-author('yoan@ytotech.com').

-callback parse(Reference :: binary(), Body :: binary(), User :: map(), Devices :: map()) -> {'ok', list(), map()} | {'error', atom()}.
