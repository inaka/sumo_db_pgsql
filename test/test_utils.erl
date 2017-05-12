-module(test_utils).

-export([start_apps/0, stop_apps/0]).

-spec start_apps() -> ok.
start_apps() ->
  {ok, _} = application:ensure_all_started(sumo_db_pgsql),
  {ok, _} = application:ensure_all_started(sumo_db),
  init_events().

-spec stop_apps() -> ok.
stop_apps() ->
  ok = application:stop(sumo_db),
  ok = application:stop(sumo_db_pgsql).

%% @private
init_events() ->
  lists:foreach(fun(EventManager) ->
    gen_event:add_handler(EventManager, EventManager, [])
  end, sumo_config:get_event_managers()).
