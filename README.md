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

Build
-----

    $ rebar3 compile

Test
-----

    $ rebar3 shell --apps geo_sensors_gateway

Develop
-----

    $ rebar3 shell
    1> {ok,Pid} = gen_smtp_server:start(smtp_server, [[
        {port, 2525},
        {sessionoptions,
            [{callbackoptions, [{auth, true}]}]
        }
        ]]).
    2> gen_smtp_server:stop(Pid).
    3> r3:do(compile).

TODO Rename project on GitHub to Erlang non-quoted atom `geo_sensors_gateway`.
