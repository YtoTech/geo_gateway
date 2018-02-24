# GeoSensors Gateway

> Receive and redistribute data from vehicle geolocation sensors.

[![Build Status](https://travis-ci.org/YtoTech/geo_sensors_gateway.svg?branch=master)](https://travis-ci.org/YtoTech/geo_sensors_gateway)
[![Hex pm](http://img.shields.io/hexpm/v/geo_sensors_gateway.svg?style=flat)](https://hex.pm/packages/geo_sensors_gateway)

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

# Installation

## Install Erlang

Erlang Solutions provides a great Erlang builds for many platforms.

Refer to their download page: https://packages.erlang-solutions.com/erlang/

### Debian-based

Recommended method: install from repository.

```
wget https://packages.erlang-solutions.com/erlang-solutions_1.0_all.deb
sudo dpkg -i erlang-solutions_1.0_all.deb
```

Then install the complete distribution:

```
sudo apt-get update
sudo apt-get install esl-erlang
```

## Build
-----

    $ make compile

## Test
-----

    $ make test

Only compatible with Erlang OTP 20+.

TODO Rename project on GitHub to Erlang non-quoted atom `geo_sensors_gateway`.

TODO Installation instructions for Erlang
* Where to get Erlang distribution
* `apt install inotify-tools` (for dev)

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
* Maybe think of another way to archive systematically any file.
	* ---> To AWS s3?
	* Create sample forwarders for that.
	* So we can recover any data when we need.
