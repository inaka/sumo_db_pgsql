-module(test_utils).

-export([start_apps/0]).

-spec start_apps() -> ok.
start_apps() ->
  {ok, _} = application:ensure_all_started(epgsql),
  {ok, _} = application:ensure_all_started(sumo_db),
  ok.
