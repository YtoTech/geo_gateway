.PHONY: test clean


mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
current_dir := $(patsubst %/,%,$(dir $(mkfile_path)))

# Development.
compile:
	rebar3 compile

dev:
	rebar3 as dev auto --apps geo_sensors_gateway

lint:
	rebar3 dialyzer

test:
	ERL_FLAGS="-config ./priv/conf/sys.config" rebar3 eunit

test-cover:
	# TODO Find how we can visualize this data. (In CLI, CI)
	ERL_FLAGS="-config ./priv/conf/sys.config" rebar3 eunit --cover

# Releasing and running in production.
release:
	rebar3 as prod release

start:
	_build/prod/rel/GeoSensorsGateway/bin/GeoSensorsGateway foreground

## Docker

docker-build:
	docker build -t geo-sensors-gateway .

docker-start:
	docker run -d -p 2525:25 -v $(current_dir)/dumps:/home/geo-sensors-gateway/dumps/ --name geo-sensors-gateway geo-sensors-gateway

docker-stop:
	docker stop geo-sensors-gateway

docker-remove:
	docker rm geo-sensors-gateway

docker-log:
	docker logs -f geo-sensors-gateway
