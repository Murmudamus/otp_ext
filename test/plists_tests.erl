%%%-----------------------------------------------------------------------------
%%% @author Roux Viljoen <>
%%% @copyright (C) 2013, Roux Viljoen
%%%-----------------------------------------------------------------------------
-module(plists_tests).

-include_lib("common_eunit/include/common_eunit.hrl").
%%------------------------------------------------------------------------------
suite() -> [{timetrap,{seconds,30}}].
%%------------------------------------------------------------------------------
init_per_suite(Config) ->
  Config.
%%------------------------------------------------------------------------------
end_per_suite(_Config) ->
  ok.
%%------------------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
  Config.
%%------------------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
  ok.
%%------------------------------------------------------------------------------
init_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({init, Config}).
%%------------------------------------------------------------------------------
end_per_testcase(TestCase, Config) ->
  ?MODULE:TestCase({'end', Config}).
%%------------------------------------------------------------------------------
groups() -> [].
%%------------------------------------------------------------------------------
all() ->
  [ test_filter
  , test_foreach
  , test_fun_crash
  , test_map
  ].
%%==============================================================================
%% Tests
%%==============================================================================
test_map({init, Config})              -> Config;
test_map({'end', _Config})            -> ok;
test_map(suite)                       -> [];
test_map(doc)                         ->
  ["Test that map/ returns list in correct order"];
test_map(Config) when is_list(Config) ->
  List0 = [10, 20, 30, 40, 50, 60, 70, 80, 90],
  List1 = [11, 21, 31, 41, 51, 61, 71, 81, 91],
  Fun   = fun(X) -> X+1 end,
  %% Test default configuration
  ?assertEqual(List1, plists:map(Fun, List0)),
  %% Test with less workers than items
  ?assertEqual(List1, plists:map(Fun, List0, 4)),
  %% Test with too many workers for given list
  ?assertEqual(List1, plists:map(Fun, List0, 20)),
  ok.
%%------------------------------------------------------------------------------
test_foreach({init, Config})              -> Config;
test_foreach({'end', _Config})            -> ok;
test_foreach(suite)                       -> [];
test_foreach(doc)                         ->
  ["Test that foreach/ processes all items"];
test_foreach(Config) when is_list(Config) ->
  Tid  = ets:new(foreach_test, [public]),
  List = [10, 20, 30, 40, 50, 60, 70, 80, 90],
  Fun  = fun(X) -> ets:insert(Tid, {X}) end,
  %% Test default configuration
  ?assertEqual(ok,    plists:foreach(Fun, List)),
  ?assertEqual(List,  lists:sort([ V || {V} <- ets:tab2list(Tid)])),
  %% clean table before running again
  true = ets:delete_all_objects(Tid),
  %% Test with less workers than items
  ?assertEqual(ok,    plists:foreach(Fun, List, 4)),
  ?assertEqual(List,  lists:sort([ V || {V} <- ets:tab2list(Tid)])),
 %% clean table before running again
  true = ets:delete_all_objects(Tid),
  %% Test with less workers than items
  ?assertEqual(ok,    plists:foreach(Fun, List, 20)),
  ?assertEqual(List,  lists:sort([ V || {V} <- ets:tab2list(Tid)])),
  %% cleanup
  ets:delete(Tid),
  ok.
%%------------------------------------------------------------------------------
test_filter({init, Config})              -> Config;
test_filter({'end', _Config})            -> ok;
test_filter(suite)                       -> [];
test_filter(doc)                         ->
  ["Test that filter/  works and returns items in correct order"];
test_filter(Config) when is_list(Config) ->
  List = [10, 20, 30, 40, 50, 60, 70, 80, 90],
  Fun  = fun(X) -> X<70 andalso X>30 end,
  %% Test default configuration
  ?assertEqual([40, 50, 60], plists:filter(Fun, List)),
  %% Test with less workers than items
  ?assertEqual([40, 50, 60], plists:filter(Fun, List, 4)),
  %% Test with too many workers for given list
  ?assertEqual([40, 50, 60], plists:filter(Fun, List, 20)),
  ok.
%%------------------------------------------------------------------------------
test_fun_crash({init, Config})              -> Config;
test_fun_crash({'end', _Config})            -> ok;
test_fun_crash(suite)                       -> [];
test_fun_crash(doc)                         ->
  ["Test that if funs crash all workers are killed"];
test_fun_crash(Config) when is_list(Config) ->
  List = [10, 20, 30, 40, 50, 60, 70, 80, 90],
  Fun  = fun(40) -> erlang:error(errors);
            (_X) -> ok
         end,
  ?assertError({errors, _}, plists:foreach(Fun, List)),
  ok.
