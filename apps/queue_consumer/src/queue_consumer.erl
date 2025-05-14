-module(queue_consumer).
-behaviour(gen_server).


-export([start_link/0]).
-export([init/1, handle_info/2, terminate/2, handle_call/3, handle_cast/2, code_change/3]).

-define(QUEUE, <<"content_uploads">>).
-record(amqp_params_network, {username, password, virtual_host, host, port = 5672, ssl_options = none, heartbeat = 0}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, Connection} = amqp_connection:start(#amqp_params_network{
        username = <<"guest">>,
        password = <<"guest">>,
        virtual_host = <<"/">>,
        host = "rabbitmq"
    }),
    {ok, Channel} = amqp_connection:open_channel(Connection),
    amqp_channel:call(Channel, #'queue.declare'{queue = ?QUEUE, durable = true}),
    amqp_channel:subscribe(Channel, #'basic.consume'{queue = ?QUEUE}, self()),
    {ok, #{channel => Channel}}.

handle_info({#'basic.deliver'{delivery_tag = Tag}, #amqp_msg{payload = Payload}}, State) ->
    try
        Json = jsx:decode(Payload, [return_maps]),
        SenderId = maps:get(<<"sender_id">>, Json),
        ReceiverId = maps:get(<<"receiver_id">>, Json),
        FileType = maps:get(<<"file_type">>, Json),
        IsPayable = maps:get(<<"is_payable">>, Json),
        BinaryData = base64:decode(maps:get(<<"binary_data">>, Json)),
        Timestamp = maps:get(<<"timestamp">>, Json),

        %% Call insert into shared_storage
        shared_storage:insert_content(SenderId, ReceiverId, FileType, IsPayable, false, BinaryData, Timestamp),

        %% Acknowledge message
        amqp_channel:cast(maps:get(channel, State), #'basic.ack'{delivery_tag = Tag}),
        {noreply, State}
    catch
        Class:Reason ->
            io:format("[Error] Failed to process message: ~p~n", [{Class, Reason}]),
            {noreply, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.