-module(sender_service_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    db_migration:run(),
    shared_storage:connect(),
    queue_consumer:start_link(),
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/sender", sender_api_handler, []}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(sender_listener, [{port, 8081}], #{env => #{dispatch => Dispatch}}),
    {ok, {{one_for_one, 5, 10}, []}}.