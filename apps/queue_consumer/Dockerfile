FROM erlang:25

WORKDIR /app

COPY . /app

RUN rebar3 compile

CMD ["_build/default/rel/queue_consumer/bin/queue_consumer", "foreground"]
