%% @doc Describing the gateway_module_receiver behaviour, which is capable
%% of receiving forwarded payloads from within Erlang.
-module(gateway_module_receiver).
-author('yoan@ytotech.com').

-callback on_payload(Payload :: map(), User :: map(), Device :: map(), Forwarder :: map()) -> ok.
