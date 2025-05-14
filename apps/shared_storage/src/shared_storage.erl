-module(shared_storage).
-include("types.hrl").

-export([connect/0, save_content/2, get_by_sender/1, mark_as_paid/1]).

%% Connect to the PostgreSQL database
connect() ->
    application:ensure_all_started(postgrex),
    case postgrex:start_link([
        {hostname, "localhost"},
        {username, "postgres"},
        {password, "postgres"},
        {database, "content_app"},
        {name, ?MODULE}
    ]) of
        {ok, _Pid} ->
            ok;
        {error, Reason} ->
            io:format("Failed to connect to the database: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Save content to the database
save_content(Metadata, FileBin) ->
    Id = erlang:unique_integer([monotonic, positive]),
    Query = "INSERT INTO content (id, sender_id, receiver_id, file_type, is_payable, binary, timestamp, is_paid) VALUES ($1, $2, $3, $4, $5, $6, $7, false)",
    Args = [
        Id,
        Metadata#content.sender_id,
        Metadata#content.receiver_id,
        Metadata#content.file_type,
        Metadata#content.is_payable,
        FileBin,
        erlang:system_time(seconds)
    ],
    case postgrex:query(?MODULE, Query, Args) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            io:format("Failed to save content: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Get content by sender ID
get_by_sender(SenderId) ->
    Query = "SELECT * FROM content WHERE sender_id = $1",
    case postgrex:query(?MODULE, Query, [SenderId]) of
        {ok, #{rows := Rows}} ->
            Rows;
        {error, Reason} ->
            io:format("Failed to fetch content by sender: ~p~n", [Reason]),
            []
    end.

%% Mark content as paid
mark_as_paid(Id) ->
    Query = "UPDATE content SET is_paid = true WHERE id = $1",
    case postgrex:query(?MODULE, Query, [Id]) of
        {ok, _Result} ->
            ok;
        {error, Reason} ->
            io:format("Failed to mark content as paid: ~p~n", [Reason]),
            {error, Reason}
    end.