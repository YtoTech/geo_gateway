compile:
	rebar3 compile

dev:
	rebar3 as dev auto --apps geo_sensors_gateway

release:
	rebar3 as prod release

start:
	_build/prod/rel/GeoSensorsGateway/bin/GeoSensorsGateway foreground

docker-build:
	docker build -t geo-sensors-gateway .

docker-start:
	docker run -d -p 2525:2525 --name geo-sensors-gateway geo-sensors-gateway

docker-stop:
	docker stop geo-sensors-gateway

docker-remove:
	docker rm geo-sensors-gateway

docker-log:
	docker logs -f geo-sensors-gateway
