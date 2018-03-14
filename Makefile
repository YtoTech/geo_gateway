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
	ERL_FLAGS="-config ./priv/conf/app.config" rebar3 eunit

test-cover:
	# TODO Find how we can visualize this data. (In CLI, CI)
	ERL_FLAGS="-config ./priv/conf/app.config" rebar3 eunit --cover

# Releasing and running in production.
release:
	rebar3 as prod release

start:
	_build/prod/rel/GeoGateway/bin/GeoGateway foreground

## Docker

docker-up:
	docker-compose up
