# Inspired by https://github.com/erlang/docker-erlang

# It misses things to fetch package by rebar.
# Maybe wget? curl?
# FROM erlang:20.0.2-alpine
FROM erlang:latest

# Install Rebar3.
# RUN mkdir -p /usr/bin
ADD https://s3.amazonaws.com/rebar3/rebar3 /usr/bin/rebar3
RUN chmod a+x /usr/bin/rebar3

# Setup Environment.
# ENV PATH=:/usr/bin/rebar3:$PATH

# Working directory.
RUN mkdir -p /home/geo-sensors-gateway
WORKDIR /home/geo-sensors-gateway

# TODO Do not copy users.json. Use another way to provide it?
# Mount directory? We can put it in a config sub-direct so we can mount it only.
# Implement a provider fetching from web?
# Local file still should be a way to get started.
COPY rebar.config rebar.lock Makefile users.json /home/geo-sensors-gateway/
COPY src/* /home/geo-sensors-gateway/src/

# Expose relevant ports.
# Switch to 25
EXPOSE 25

# TODO Use rebar3 as prod release
CMD ["rebar3", "as", "prod", "shell", "--apps", "geo_sensors_gateway"]
