%%%-------------------------------------------------------------------
%% @doc geo_sensors_gateway public API
%% @end
%%%-------------------------------------------------------------------

-module(geo_sensors_gateway_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("HELLO from there~n"),
    {ok,_} = gen_smtp_server:start(smtp_server, [[
        {port, 2525},
        {sessionoptions,
            [{callbackoptions,
                [
                    {auth, true},
                    {parse, true},
                    {dump, true}
                ]
            }]
        }
        ]]),
    geo_sensors_gateway_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
