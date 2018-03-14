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
RUN mkdir -p /home/geo-gateway
WORKDIR /home/geo-gateway

COPY rebar.config rebar.lock Makefile /home/geo-gateway/
COPY src/* /home/geo-gateway/src/
COPY priv/conf/* /home/geo-gateway/priv/conf/

RUN make release

# Expose relevant ports.
EXPOSE 25

CMD ["make", "start"]
