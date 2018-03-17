%% @doc Tests for geo_gateway_forwarder_http module.

-module(geo_gateway_forwarder_http_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

ddm_to_decimal_test_() ->
	[
		{"Float conversion for GET parameter encoding",
		[
			?_assertEqual(<<"longitude=0.774004">>, erlang:iolist_to_binary(io_lib:format("longitude=~f", [0.7740036666666666]))),
			?_assertEqual(<<"longitude=0.000815">>, erlang:iolist_to_binary(io_lib:format("longitude=~f", [8.153333333333333e-4])))
		]}
	].
