%% @doc Describing the geo_gateway_module_payload_receiver behaviour, which is capable
%% of receiving forwarded payloads from within Erlang.
-module(geo_gateway_module_payload_receiver).
-author('yoan@ytotech.com').

-callback on_payload(Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> ok.
