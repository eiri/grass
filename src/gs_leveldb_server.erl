-module(gs_leveldb_server).

-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ctx, {ref, dir, opts}).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop(Ref) ->
  gen_server:call(Ref, stop).

init([WorkDir]) ->
  Opts = [{create_if_missing, true}, {compression, true}, {verify_compactions, true}],
  case eleveldb:open(WorkDir, Opts) of
    {ok, Ref} ->
      {ok, #ctx{ref = Ref, dir = WorkDir, opts = Opts}};
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
handle_call(stop, _From, #ctx{ref = Ref} = Ctx) ->
  {stop, normal, ok, Ctx}.

handle_cast({put, Key, Value}, #ctx{ref = Ref} = Ctx) ->
  ok = eleveldb:put(Ref, Key, msgpack:pack(Value, [jsx]), []),
  {noreply, Ctx};
handle_cast({delete, Key}, #ctx{ref = Ref} = Ctx) ->
  ok = eleveldb:delete(Ref, Key, []),
  {noreply, Ctx};
handle_cast(drop, #ctx{ref = Ref, dir = Dir, opts = Opts} = Ctx) ->
  eleveldb:close(Ref),
  ok = eleveldb:destroy(Dir, []),
  {ok, NewRef} = eleveldb:open(Dir, Opts),
  {noreply, Ctx#ctx{ref = NewRef}}.

handle_info(Msg, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, #ctx{ref = Ref}) ->
  eleveldb:close(Ref),
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.
