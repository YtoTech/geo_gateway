# Geo Sensors Gateway

> Receive and redistribute data from vehicle geolocation sensors.

[![Build Status](https://travis-ci.org/YtoTech/geo_gateway.svg?branch=master)](https://travis-ci.org/YtoTech/geo_gateway)
[![Hex pm](http://img.shields.io/hexpm/v/geo_gateway.svg?style=flat)](https://hex.pm/packages/geo_gateway)

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

To control the build process, you can also use [kerl](https://github.com/kerl/kerl).

### Example for Debian

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

## Install Rebar3

See [official installation instructions](https://www.rebar3.org/docs/getting-started#section-installing-binary).

The easiest is to download the latest binary build of Rebar3, place in a directory
and add it to your path.

```
# These instructions are only provided as example: please refer to official instructions.
# This is a system-wide install.
sudo wget https://s3.amazonaws.com/rebar3/rebar3 -O /usr/bin/rebar3
sudo chmod +x /usr/bin/rebar3
```

# Use it

## Create a configuration file

You can use your configuration file from a sample to get started rapidly.

```sh
cp ./priv/conf/configuration.sample.json ./priv/conf/configuration.json
```

## Build
-----

    $ make compile

## Test
-----

    $ make test

Only compatible with Erlang OTP 20+.

TODO Installation instructions for Erlang
* Where to get Erlang distribution
* `apt install inotify-tools` (for dev)

% TODO Use error_logger:info_msg("") for logging
--> Use a logging facility and be able to configure it to stdout or file,
with a defined verbosity. (let message dump in stdout only in debug)

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
* Gateway reporting: add a statistics service
	* Report to Slack or mails the number of payloads forwarder in a day, week, etc.
	* Manage that in the application level? (Geo-service storage API?)
