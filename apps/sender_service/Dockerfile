FROM erlang:25

WORKDIR /app
COPY . /app

RUN rebar3 compile

CMD ["_build/default/rel/sender_service/bin/sender_service", "foreground"]
