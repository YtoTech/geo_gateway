compile:
	rebar3 compile

test:
	rebar3 shell --apps geo_sensors_gateway

dev:
	rebar3 auto --apps geo_sensors_gateway
