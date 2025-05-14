-module(sender_api_handler).
-behaviour(cowboy_handler).

-record(content, {
    sender_id,
    receiver_id,
    file_type,
    is_payable
}).

-export([init/2]).

init(Req, State) ->
    case cowboy_req:read_body(Req) of
        {ok, Body, Req1} ->
            case catch parse_json_and_extract(Body) of
                {ok, Metadata, FileBin} ->
                    case shared_storage:save_content(Metadata, FileBin) of
                        ok ->
                            ok = queue_consumer:publish(Metadata#content.receiver_id),
                            {ok, cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"{\"status\":\"ok\"}">>, Req1), State};
                        {error, Reason} ->
                            {ok, cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req1), State}
                    end;
                {error, Reason} ->
                    {ok, cowboy_req:reply(400, #{<<"content-type">> => <<"application/json">>}, jsx:encode(#{error => Reason}), Req1), State}
            end;
        {error, _Reason} ->
            {ok, cowboy_req:reply(500, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"internal_server_error\"}">>, Req), State}
    end.

parse_json_and_extract(Body) ->
    case jsx:decode(Body, [return_maps]) of
        #{<<"sender_id">> := SenderId, <<"receiver_id">> := ReceiverId, <<"file_type">> := FileType, <<"is_payable">> := IsPayable, <<"file">> := FileBin} ->
            Metadata = #content{
                sender_id = SenderId,
                receiver_id = ReceiverId,
                file_type = FileType,
                is_payable = IsPayable
            },
            {ok, Metadata, FileBin};
                _ ->
                    {error, <<"invalid_json_format">>}
        end.