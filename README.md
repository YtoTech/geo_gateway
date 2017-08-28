# GeoSensors Gateway

> Receive and redistribute data from vehicle geolocation sensors.

Prototyping project to receive data from remote Ercogener Genloc sensors.

Try to receive data from TCP:
* not message-based, but we can presume that one connection = one message;
* how to secure the channel and protect from fake-data?
* very light still reliable.

Try to receive data from SMTP:
* certainly can put a password;
* clearly message-based;
* still light;
* maybe difficult if we got a lot of sensors on the gateway.

Do a server prototype on Erlang, so we really begin to construct reliable
pieces of network and messaging software.

An OTP application

## Build
-----

    $ make compile

## Test
-----

    $ make test


TODO Rename project on GitHub to Erlang non-quoted atom `geo_sensors_gateway`.

% TODO Use error_logger:info_msg("") for logging

TODO Put examples in /doc/examples cf http://erlang.org/doc/design_principles/applications.html

TODO Add unit test for:
* Auth
    * Username does not match
    * Plain, Login, cram-md5
* FROM filtering
* Json user loading
	* Missing password, email
* Mail parsing
	* Invalid mail
	* Sensor not managed
	* Valid mail for Ercogener GenLoc
TODO Forwarder examples:
* dumping (raw and parsed);
* to a list of mails;
* to a Google Sheet;
* to https://www.mapbox.com/api-documentation/#datasets
* POST to custom API endpoint
