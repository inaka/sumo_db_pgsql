-module(find_SUITE).

%% CT
-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

%% Test Cases
-include_lib("mixer/include/mixer.hrl").
-mixin([{sumo_find_test_helper, [find_by_sort/1, find_all_sort/1]}]).

-define(EXCLUDED_FUNS, [
  module_info,
  all,
  init_per_suite,
  end_per_suite
]).

-type config() :: [{atom(), term()}].

%%%=============================================================================
%%% Common test
%%%=============================================================================

-spec all() -> [atom()].
all() ->
  Exports = ?MODULE:module_info(exports),
  [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
  ok = test_utils:start_apps(),
  ok = sumo_find_test_helper:init_store(people),
  [{name, people} | Config].

-spec end_per_suite(config()) -> config().
end_per_suite(Config) ->
  ok = test_utils:stop_apps(),
  Config.
