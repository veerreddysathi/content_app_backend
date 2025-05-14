FROM erlang:25

WORKDIR /app

COPY . /app

RUN rebar3 compile

CMD ["_build/default/rel/consumer_service/bin/consumer_service", "foreground"]
