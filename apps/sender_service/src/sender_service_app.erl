-module(sender_service_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    sender_service_sup:start_link().

stop(_State) ->
    ok.