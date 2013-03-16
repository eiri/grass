-module(gs_graph_server).

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ctx, {ref, name, dir, opts}).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

stop(Ref) ->
  gen_server:call(Ref, stop).

init([{Name, WorkDir}]) ->
  GraphDir = filename:join(WorkDir, "verticies"),
  Opts = [{create_if_missing, true}, {compression, true}, {verify_compactions, true}],
  case eleveldb:open(GraphDir, Opts) of
    {ok, Ref} ->
      gs_register_server:set(Name, self()),
      {ok, #ctx{name = Name, ref = Ref, dir = GraphDir, opts = Opts}};
    {error, Reason} ->
      {error, Reason}
  end.

handle_call(all, _From, #ctx{ref = Ref} = Ctx) ->
  Fun = fun({K, MSV}, Acc) ->
    {ok, V} = msgpack:unpack(MSV, [jsx]),
    [{K, V}] ++ Acc
  end,
  All = eleveldb:fold(Ref, Fun, [], []),
  {reply, lists:reverse(All), Ctx};
handle_call({all, StartFrom}, _From, #ctx{ref = Ref} = Ctx) ->
  Fun = fun({K, MSV}, Acc) ->
    {ok, V} = msgpack:unpack(MSV, [jsx]),
    [{K, V}] ++ Acc
  end,
  All = eleveldb:fold(Ref, Fun, [], [{first_key, StartFrom}]),
  {reply, lists:reverse(All), Ctx};
handle_call(keys, _From, #ctx{ref = Ref} = Ctx) ->
  AccFun = fun(K, A) -> [K|A] end,
  All = eleveldb:fold_keys(Ref, AccFun, [], []),
  {reply, lists:reverse(All), Ctx};
handle_call({keys, StartFrom}, _From, #ctx{ref = Ref} = Ctx) ->
  AccFun = fun(K, A) -> [K|A] end,
  All = eleveldb:fold_keys(Ref, AccFun, [], [{first_key, StartFrom}]),
  {reply, lists:reverse(All), Ctx};
handle_call({exists, Key}, _From, #ctx{ref = Ref} = Ctx) ->
  case eleveldb:get(Ref, Key, []) of
      not_found -> {reply, false, Ctx};
      {error, Error} -> {reply, {error, {eleveldb, Error}}, Ctx};
      {ok, _} -> {reply, true, Ctx}
  end;
handle_call({get, Key}, _From, #ctx{ref = Ref} = Ctx) ->
  case eleveldb:get(Ref, Key, []) of
    not_found -> {reply, {error, not_found}, Ctx};
    {error, Error} -> {reply, {error, {eleveldb, Error}}, Ctx};
    {ok, MPValue} ->
      {ok, Value} = msgpack:unpack(MPValue, [jsx]),
      {reply, {ok, Key, Value}, Ctx}
  end;
handle_call(stats, _From, #ctx{ref = Ref} = Ctx) ->
  {ok, Stats} = eleveldb:status(Ref, <<"leveldb.stats">>),
  {reply, Stats, Ctx};
handle_call(is_empty, _From, #ctx{ref = Ref} = Ctx) ->
  {reply, eleveldb:is_empty(Ref), Ctx};
handle_call(drop, _From, #ctx{ref = Ref, dir = Dir, opts = Opts} = Ctx) ->
  eleveldb:close(Ref),
  ok = eleveldb:destroy(Dir, []),
  {ok, NewRef} = eleveldb:open(Dir, Opts),
  {reply, ok, Ctx#ctx{ref = NewRef}};
handle_call(destroy, _From, #ctx{ref = Ref, dir = Dir} = Ctx) ->
  eleveldb:close(Ref),
  Reply = eleveldb:destroy(Dir, []),
  {reply, Reply, Ctx#ctx{ref = undefined}};
handle_call(stop, _From, Ctx) ->
  {stop, normal, ok, Ctx}.

handle_cast({put, Key, Value}, #ctx{ref = Ref} = Ctx) ->
  ok = eleveldb:put(Ref, Key, msgpack:pack(Value, [jsx]), []),
  {noreply, Ctx};
handle_cast({delete, Key}, #ctx{ref = Ref} = Ctx) ->
  ok = eleveldb:delete(Ref, Key, []),
  {noreply, Ctx}.

handle_info(_Msg, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, #ctx{name = Name, ref = undefined}) ->
  gs_register_server:del(Name),
  ok;
terminate(_Reason, #ctx{name = Name, ref = Ref}) ->
  eleveldb:close(Ref),
  gs_register_server:del(Name),
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.
