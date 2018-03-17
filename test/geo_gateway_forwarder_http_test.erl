%% @doc Tests for geo_gateway_forwarder_http module.

-module(geo_gateway_forwarder_http_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

decimal_formatting_test_() ->
	[
		{"Float conversion for GET parameter encoding",
		[
			?_assertEqual(<<"0.774003666666667">>, float_to_binary(0.7740036666666666, geo_gateway_forwarder_http:float_format())),
			?_assertEqual(<<"0.000815333333333">>, float_to_binary(8.153333333333333e-4, geo_gateway_forwarder_http:float_format()))
		]},
		{"Float conversion for JSON parameter encoding",
		[
			?_assertEqual(<<"{\"longitude\":0.774003666666667}">>, jsone:encode(#{ longitude => 0.7740036666666666 }, geo_gateway_forwarder_http:json_options())),
			?_assertEqual(<<"{\"longitude\":0.000815333333333}">>, jsone:encode(#{ longitude => 8.153333333333333e-4 }, geo_gateway_forwarder_http:json_options()))
		]}
	].
