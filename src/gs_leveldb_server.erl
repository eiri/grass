-module(gs_leveldb_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ctx, {ref, dir, opts}).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

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
handle_call({get, Key}, _From, #ctx{ref = Ref} = Ctx) ->
  {ok, MSValue} = eleveldb:get(Ref, Key, []),
  {ok, Value} = msgpack:unpack(MSValue, [jsx]),
  {reply, Value, Ctx};
handle_call(stats, _From, #ctx{ref = Ref} = Ctx) ->
  {ok, Stats} = eleveldb:status(Ref, <<"leveldb.stats">>),
  {reply, Stats, Ctx};
handle_call(is_empty, _From, #ctx{ref = Ref} = Ctx) ->
  {reply, eleveldb:is_empty(Ref), Ctx}.

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
  lager:info("got ~p", [Msg]),
  {noreply, Ctx}.

terminate(_Reason, #ctx{ref = Ref}) ->
  eleveldb:close(Ref),
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.
