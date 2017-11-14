%% @doc Tests for geo_conversions module.

-module(geo_conversions_tests).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

ddm_to_decimal_test_() ->
	[
		{"Convert nominal values",
		[
			?_assertEqual(44.572409, geo_conversions:ddm_to_decimal(<<"4434.34454">>, <<"N">>)),
			?_assertEqual(0.7740036666666666, geo_conversions:ddm_to_decimal(<<"00046.44022">>, <<"E">>)),
			?_assertEqual(52.31773616666667, geo_conversions:ddm_to_decimal(<<"5219.06417">>, <<"N">>)),
			?_assertEqual(9.799914666666666, geo_conversions:ddm_to_decimal(<<"00947.99488">>, <<"E">>))	
		]}
	].
