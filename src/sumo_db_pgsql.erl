-module(sumo_db_pgsql).

-behaviour(application).

-export([start/0, start/2, stop/1]).

-spec start() -> {ok, [term()]}.
start() -> {ok, _} = application:ensure_all_started(sumo_db_pgsql).

-spec start(StartType::application:start_type(), StartArgs::term()) ->
  {ok, pid()}.
start(_StartType, _StartArgs) -> {ok, self()}.

-spec stop(State::term()) -> ok.
stop(_State) -> ok.
