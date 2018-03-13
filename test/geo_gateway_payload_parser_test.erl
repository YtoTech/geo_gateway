%% @doc Tests for geo_gateway_payload_parser_tests.

-module(geo_gateway_payload_parser_test).
-author('yoan@ytotech.com').
-include_lib("eunit/include/eunit.hrl").

parse_genloc_gprmc_test_() ->
	[
		{"Parse a payload of typical Genloc $GPRMC messages",
		fun() ->
			Body = <<"        $GPRMC,163734.00,A,4434.34454,N,00046.44022,E,0.015,0.00,230917,,,A*67\n"
			"        $GPRMC,092733.00,A,5219.06325,N,00947.99798,E,0.053,0.00,121117,,,A*60\n"
			"        $GPRMC,144731.00,A,5013.07991,N,00314.61000,E,67.822,50.66,101117,,,A*61">>,
			User = #{
				device => <<"ercogener_genloc_341e">>,
				dumps_incoming => true,
				email => <<"test@ytotech.com">>,
				forwarders => [<<"file_dump">>],
				password => <<"coincoin">>
			},
			Devices = #{
				<<"ercogener_genloc_341e">> => #{
					manufacturer => <<"Ercogener">>,
					model => <<"451e EaseLoc">>,
					parameters => #{},
					range => <<"GenLoc">>
				}
			},
			Result = geo_gateway_payload_parser:parse("", Body, User, Devices),
			{Status, ResultPayloadItems, Device} = Result,
			?assertEqual(ok, Status),
			ParsedExpected = [
				{ok, #{
					date => {2017,9,23},
					format => <<"GPRMC">>,
					latitude => 44.572409,
					longitude => 0.7740036666666666,
					raw => <<"$GPRMC,163734.00,A,4434.34454,N,00046.44022,E,0.015,0.00,230917,,,A*67">>,
					status => <<"A">>,
					time => {16,37,34.0},
					timestamp => {{2017,9,23},{16,37,34.0}}
				}},
				{ok,#{
					date => {2017,11,12},
					format => <<"GPRMC">>,
					latitude => 52.31772083333333,
					longitude => 9.799966333333334,
					raw => <<"$GPRMC,092733.00,A,5219.06325,N,00947.99798,E,0.053,0.00,121117,,,A*60">>,
					status => <<"A">>,
					time => {9,27,33.0},
					timestamp => {{2017,11,12},{9,27,33.0}}
				}},
				{ok,#{
					date => {2017,11,10},
					format => <<"GPRMC">>,
					latitude => 50.2179985,
					longitude => 3.2435,
					raw => <<"$GPRMC,144731.00,A,5013.07991,N,00314.61000,E,67.822,50.66,101117,,,A*61">>,
					status => <<"A">>,
					time => {14,47,31.0},
					timestamp => {{2017,11,10},{14,47,31.0}}
				}}
			],
			?assertEqual(3, length(ResultPayloadItems)),
			?assertEqual(maps:get(<<"ercogener_genloc_341e">>, Devices), Device),
			?assertEqual(lists:nth(1, ParsedExpected), lists:nth(1, ResultPayloadItems)),
			?assertEqual(lists:nth(2, ParsedExpected), lists:nth(2, ResultPayloadItems)),
			?assertEqual(lists:nth(3, ParsedExpected), lists:nth(3, ResultPayloadItems))
		end}
	].

parse_genloc_gploc_test_() ->
	[
		{"Parse a payload of typical Genloc $GPLOC messages",
		fun() ->
			Body = <<" $GPLOC,358683066123549,A,1,125807.00,4834.37575,N,00105.99399,W,00000,3181000000003775*20\n"
			" $GPLOC,358683066123549,A,1,081247.00,4834.36782,N,00105.95857,W,01100,3194857900003819*2D">>,
			User = #{
				device => <<"ercogener_genloc_341e">>,
				dumps_incoming => true,
				email => <<"test@ytotech.com">>,
				forwarders => [<<"file_dump">>],
				password => <<"coincoin">>
			},
			Devices = #{
				<<"ercogener_genloc_341e">> => #{
					manufacturer => <<"Ercogener">>,
					model => <<"451e EaseLoc">>,
					parameters => #{},
					range => <<"GenLoc">>
				}
			},
			Result = geo_gateway_payload_parser:parse("", Body, User, Devices),
			{Status, ResultPayloadItems, Device} = Result,
			?assertEqual(ok, Status),
			ParsedExpected = [
				{ok, #{
					date => nil,
					fix_quality => <<"1">>,
					format => <<"GPLOC">>,
					latitude => 48.57292916666667,
					longitude => -1.0998998333333334,
					raw => <<"$GPLOC,358683066123549,A,1,125807.00,4834.37575,N,00105.99399,W,00000,3181000000003775*20">>,
					status => <<"A">>,
					time => {12,58,7.0},
					timestamp => nil
				}},
				{ok,#{
					date => nil,
					fix_quality => <<"1">>,
					format => <<"GPLOC">>,
					latitude => 48.572797,
					longitude => -1.0993095,
					raw => <<"$GPLOC,358683066123549,A,1,081247.00,4834.36782,N,00105.95857,W,01100,3194857900003819*2D">>,
					status => <<"A">>,
					time => {8,12,47.0},
					timestamp => nil
				}}
			],
			?assertEqual(2, length(ResultPayloadItems)),
			?assertEqual(maps:get(<<"ercogener_genloc_341e">>, Devices), Device),
			?assertEqual(lists:nth(1, ParsedExpected), lists:nth(1, ResultPayloadItems)),
			?assertEqual(lists:nth(2, ParsedExpected), lists:nth(2, ResultPayloadItems))
		end}
	].
