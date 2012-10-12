-module(gs_leveldb_server).

-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ctx, {ref}).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

init([WorkDir]) ->
  case eleveldb:open(WorkDir, [{create_if_missing, true}]) of
    {ok, Ref} ->
      {ok, #ctx{ref = Ref}};
    {error, Reason} ->
      {error, Reason}
  end.

handle_call(all, _From, #ctx{ref = Ref} = Ctx) ->
  Fun = fun({K, V}, Acc) ->
    [{K, V}] ++ Acc
  end,
  All = eleveldb:fold(Ref, Fun, [], []),
  {reply, All, Ctx};
handle_call({get, Key}, _From, #ctx{ref = Ref} = Ctx) ->
  Value = eleveldb:get(Ref, Key, []),
  {reply, Value, Ctx};
handle_call(stats, _From, #ctx{ref = Ref} = Ctx) ->
  {ok, Stats} = eleveldb:status(Ref, <<"leveldb.stats">>),
  {reply, Stats, Ctx}.

handle_cast({put, Key, Value}, #ctx{ref = Ref} = Ctx) ->
  ok = eleveldb:put(Ref, Key, Value, []),
  {noreply, Ctx};
handle_cast({delete, Key}, #ctx{ref = Ref} = Ctx) ->
  ok = eleveldb:delete(Ref, Key, []),
  {noreply, Ctx}.

handle_info(Msg, Ctx) ->
  lager:info("got ~p", [Msg]),
  {noreply, Ctx}.

terminate(_Reason, #ctx{ref = Ref}) ->
  eleveldb:close(Ref),
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.
