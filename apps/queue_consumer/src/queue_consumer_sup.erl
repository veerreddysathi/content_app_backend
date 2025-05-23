-module(queue_consumer_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    queue_consumer:start_link(),
    {ok, {{one_for_one, 5, 10}, []}}.