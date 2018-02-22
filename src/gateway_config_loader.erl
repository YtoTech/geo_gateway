%% @doc Describing the gateway_config_loader behaviour, which is responsible
%% for loading and exposing the gateway configuration.
%%
%% It may uses any source (process dictionnary, configuration file, database,
%% HTTP service).
-module(gateway_config_loader).
-author('yoan@ytotech.com').

-callback load_config() -> #{devices => map(), forwarders => map(), smtp_gateway => map(), users => map()}.
