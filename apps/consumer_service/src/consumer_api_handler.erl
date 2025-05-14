-module(consumer_api_handler).
-behaviour(cowboy_handler).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:method(Req) of
        <<"GET">> ->
            handle_get_request(Req, State);
        <<"POST">> ->
            handle_post_request(Req, State);
        _ ->
            {ok, cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"method_not_allowed\"}">>, Req), State}
    end.

handle_get_request(Req, State) ->
    case cowboy_req:qs_val(<<"sender_id">>, Req) of
        {ok, SenderIdStr} ->
            case catch list_to_integer(binary_to_list(SenderIdStr)) of
                SenderId when is_integer(SenderId) ->
                    Contents = shared_storage:get_by_sender(SenderId),
                    Body = jsx:encode(#{contents => Contents}),
                    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, Body, Req), State};
                _ ->
                        {ok, cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"invalid_sender_id\"}">>, Req), State}
                end;
        _ ->
                {ok, cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"missing_sender_id\"}">>, Req), State}
    end.

handle_post_request(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req1} ->
            case catch list_to_integer(binary_to_list(Body)) of
                Id when is_integer(Id) ->
                    shared_storage:mark_as_paid(Id),
                    {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"{\"status\":\"paid\"}">>, Req1), State};
                _ ->
                    {ok, cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"invalid_id\"}">>, Req1), State}
            end;
        {error, _Reason} ->
            {ok, cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"internal_server_error\"}">>, Req), State}
    end.