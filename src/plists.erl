%%%-----------------------------------------------------------------------------
%%% @author Roux Viljoen <>
%%% @copyright (C) 2013, Roux Viljoen
%%% @doc List functions that run in parallel
%%% @end
%%%-----------------------------------------------------------------------------
-module(plists).

%% API
-export([ filter/2
        , filter/3
        , foreach/2
        , foreach/3
        , map/2
        , map/3
        ]).

-record(cfg, {exec_fun,
              workers       :: integer(),
              return_result :: boolean(),
              index = 0     :: integer()
             }).

-define(EMPTY, '_empty_result_').
-record(result, {pid,
                 data = ?EMPTY
                }).
-define(DEFAULT_WORKERS, 5).
%%%=============================================================================
%%% API
%%%=============================================================================
map(Fun, List) -> map(Fun, List, ?DEFAULT_WORKERS).

map(Fun, List, WorkerCount) when WorkerCount > 0 andalso
                                 is_integer(WorkerCount) andalso
                                 is_list(List) andalso
                                 is_function(Fun, 1) ->
  Cfg = #cfg{exec_fun      = fun({Index, Item}) -> {Index, Fun(Item)} end,
             workers       = WorkerCount,
             return_result = true
            },
  start(List, Cfg).
%%------------------------------------------------------------------------------
foreach(Fun, List) -> foreach(Fun, List, ?DEFAULT_WORKERS).

foreach(Fun, List, WorkerCount) when WorkerCount > 0 andalso
                                     is_integer(WorkerCount) andalso
                                     is_list(List) andalso
                                     is_function(Fun, 1) ->
  Cfg = #cfg{exec_fun      = Fun,
             workers       = WorkerCount,
             return_result = false
            },
  start(List, Cfg).
%%------------------------------------------------------------------------------
filter(Fun, List) -> filter(Fun, List, ?DEFAULT_WORKERS).

filter(Fun, List, WorkerCount) when WorkerCount > 0 andalso
                                    is_integer(WorkerCount) andalso
                                    is_list(List) andalso
                                    is_function(Fun, 1) ->
  FilterFun = fun({Index, Item}) ->
                  case Fun(Item) of
                    false -> ?EMPTY;
                    true  -> {Index, Item}
                  end
              end,
  Cfg       = #cfg{exec_fun      = FilterFun,
                   workers       = WorkerCount,
                   return_result = true
                  },
  start(List, Cfg).
%%%=============================================================================
%%% Internal functions
%%%=============================================================================
start(List, Cfg) ->
  Self = self(),
  Fun  = fun() ->
             process_flag(trap_exit, true),
             _MonitorRef = erlang:monitor(process, Self),
             Result      = #result{pid  = self(),
                                   data = spawn_spawner(List, Cfg)
                                  },
             send_data(Self, Result)
         end,
  {Pid, Ref} = erlang:spawn_monitor(Fun),
  receive
    {'DOWN', Ref, process, Pid, normal} -> get_result(Pid);
    {'DOWN', Ref, process, Pid, Reason} -> erlang:error(Reason)
  end.
%%------------------------------------------------------------------------------
send_data(Pid, Data) ->
  Pid ! Data,
  ok.
%%------------------------------------------------------------------------------
get_result(Pid) ->
  receive
    #result{pid=Pid} = Result -> Result#result.data
  end.
%%------------------------------------------------------------------------------
spawn_spawner(List, Cfg) ->
  MasterPid        = self(),
  WorkerSpawnerFun = fun() ->
                         process_flag(trap_exit, true),
                         spawn_workers(MasterPid, Cfg)
                     end,
  WorkerSpawnerPid = erlang:spawn_link(WorkerSpawnerFun),
  master_wait_loop(List, WorkerSpawnerPid, Cfg, []).
%%------------------------------------------------------------------------------
master_wait_loop(List, WorkerSpawnerPid, Cfg0, Results) ->
  MasterPid = self(),
  {H, T, Cfg1} = pack(List, Cfg0),
  receive
    #result{pid=WorkerSpawnerPid, data=stop} when List =:= [] ->
      unpack(Results, Cfg1);
    #result{} = R when List =:= []                            ->
      send_data(R#result.pid, {stop, MasterPid}),
      master_wait_loop([], WorkerSpawnerPid, Cfg1, update_result(R, Results));
    #result{} = R                                             ->
      send_data(R#result.pid, {chunk, MasterPid, H}),
      master_wait_loop(T, WorkerSpawnerPid, Cfg1,  update_result(R, Results));
    %% spawner died
    {'EXIT', WorkerSpawnerPid, Reason}                        -> exit(Reason)
  end.
%%------------------------------------------------------------------------------
spawn_workers(MasterPid, Cfg) ->
  spawn_workers(Cfg#cfg.workers, Cfg, MasterPid, []).

spawn_workers(0, _Cfg, MasterPid, [])     ->
  send_data(MasterPid, #result{pid=self(), data=stop});
spawn_workers(0, Cfg, MasterPid, PidList) ->
  receive
    {'EXIT', Pid, normal}  ->
      spawn_workers(0, Cfg, MasterPid, lists:delete(Pid, PidList));
    {'EXIT', _Pid, Reason} ->
      lists:foreach(fun(Worker) ->
                        send_data(Worker, {stop, MasterPid}) end,
                    PidList),
      exit(Reason)
  end;
spawn_workers(WorkerCount, Cfg, MasterPid, PidList) ->
  WorkerFun = fun() ->
                  send_data(MasterPid, #result{pid=self()}),
                  worker_wait_loop(MasterPid, Cfg)
              end,
  WorkerPid = erlang:spawn_link(WorkerFun),
  spawn_workers(WorkerCount-1, Cfg, MasterPid, [WorkerPid|PidList]).
%%------------------------------------------------------------------------------
worker_wait_loop(MasterPid, Cfg) ->
  receive
    {chunk, MasterPid, Data} ->
      Result = return_result(Cfg, (Cfg#cfg.exec_fun)(Data)),
      send_data(MasterPid, #result{pid=self(), data=Result}),
      worker_wait_loop(MasterPid, Cfg);
    {stop, MasterPid}        -> ok
  end.
%%------------------------------------------------------------------------------
pack([], Cfg)    -> {[], [], Cfg};
pack([H|T], Cfg) ->
  case Cfg#cfg.return_result of
    false -> {H, T, Cfg};
    true  ->
      Index = Cfg#cfg.index,
      {{Index, H}, T, Cfg#cfg{index=Index+1}}
  end.
%%------------------------------------------------------------------------------
unpack(List, #cfg{return_result=true}) ->
  {_Index, Results} = lists:unzip(lists:keysort(1, List)),
  Results;
unpack(_List, _Cfg)                    -> ok.
%%------------------------------------------------------------------------------
return_result(Cfg, Result) ->
  case Cfg#cfg.return_result of
    false -> ?EMPTY;
    true  -> Result
  end.
%%------------------------------------------------------------------------------
update_result(#result{data=?EMPTY}, Results) -> Results;
update_result(R, Results)                    -> [R#result.data|Results].
%%------------------------------------------------------------------------------
