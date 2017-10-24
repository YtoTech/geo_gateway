# Development.
compile:
	rebar3 compile

dev:
	rebar3 as dev auto --apps geo_sensors_gateway

lint:
	rebar3 dialyzer

test:
	rebar3 eunit

# Releasing and running in production.
release:
	rebar3 as prod release

start:
	_build/prod/rel/GeoSensorsGateway/bin/GeoSensorsGateway foreground

## Docker

docker-build:
	docker build -t geo-sensors-gateway .

docker-start:
	docker run -d -p 2525:25 --name geo-sensors-gateway geo-sensors-gateway

docker-stop:
	docker stop geo-sensors-gateway

docker-remove:
	docker rm geo-sensors-gateway

docker-log:
	docker logs -f geo-sensors-gateway
