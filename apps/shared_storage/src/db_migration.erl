-module(db_migration).
-export([run/0]).

run() ->
    application:ensure_all_started(epgsql),
    {ok, Conn} = epgsql:connect("localhost", "postgres", "postgres", #{database => "content_app"}),
    run_sql_file(Conn, "../migrations/001_create_content_table.sql"),
    epgsql:close(Conn),
    ok.

run_sql_file(Conn, File) ->
    {ok, Sql} = file:read_file(File),
    case epgsql:squery(Conn, binary_to_list(Sql)) of
        {ok, _} -> io:format("Migration ~s applied successfully~n", [File]);
        {error, Reason} -> io:format("Error running migration ~s: ~p~n", [File, Reason])
    end.