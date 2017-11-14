%% @doc Tests for device_payload_parser_example_tests.

-module(device_payload_parser_example_test).
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
			Result = device_payload_parser_example:parse("", Body, User, Devices),
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
