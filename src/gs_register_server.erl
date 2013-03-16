-module(gs_register_server).

-behaviour(gen_server).

-export([start_link/1, stop/0, create/1, destroy/1, get/0, get/1, set/2, del/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(ctx, {tid, dir}).

start_link(Args) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
  gen_server:call(?MODULE, stop).

init([Tid, WorkDir]) ->
  %% yes, it's a race condition, I know
  {ok, #ctx{tid = Tid, dir = WorkDir}, 200}.

create(G) ->
  gen_server:call(?MODULE, {create, G}).

destroy(G) ->
  gen_server:cast(?MODULE, {destroy, G}).

get() ->
  gen_server:call(?MODULE, get).

get(G) ->
  gen_server:call(?MODULE, {get, G}).

set(G, Pid) ->
  gen_server:cast(?MODULE, {set, G, Pid}).

del(G) ->
  gen_server:cast(?MODULE, {del, G}).

handle_call({create, G}, _From, #ctx{dir = WorkDir} = Ctx) ->
  GraphDir = filename:join(WorkDir, binary_to_list(G)),
  file:make_dir(GraphDir),
  case gs_graph_sup:start_child(G, GraphDir) of
    {ok, _} -> {reply, ok, Ctx};
    Err -> {reply, {error, Err}, Ctx}
  end;
handle_call(get, _From, #ctx{tid = Tid} = Ctx) ->
  Reply = [ {G, Pid} || [{G, Pid}] <- ets:match(Tid, '$1') ],
  {reply, lists:usort(Reply), Ctx};
handle_call({get, G}, _From, #ctx{tid = Tid} = Ctx) ->
  case ets:lookup(Tid, G) of
    [] ->
      {reply, {error, not_found}, Ctx};
    [{G, Pid}] ->
      {reply, {ok, Pid}, Ctx}
  end;
handle_call(stop, _From, Ctx) ->
  {stop, normal, ok, Ctx}.

handle_cast({destroy, G}, #ctx{tid = Tid, dir = WorkDir} = Ctx) ->
  case ets:lookup(Tid, G) of
    [{G, Pid}] ->
      gen_server:call(Pid, destroy),
      gen_server:call(Pid, stop),
      gs_graph_sup:stop_child(G),
      GraphDir = filename:join(WorkDir, binary_to_list(G)),
      file:del_dir(GraphDir);
    _ -> ok
  end,
  {noreply, Ctx};
handle_cast({set, G, Pid}, #ctx{tid = Tid} = Ctx) ->
  ets:insert(Tid, {G, Pid}),
  {noreply, Ctx};
handle_cast({del, G}, #ctx{tid = Tid} = Ctx) ->
  ets:delete(Tid, G),
  {noreply, Ctx};
handle_cast(_, Ctx) ->
  {noreply, Ctx}.

handle_info(timeout, #ctx{dir = WorkDir} = Ctx) ->
  {ok, List} = file:list_dir(WorkDir),
  Load = fun(Dir) ->
    GraphDir = filename:join(WorkDir, Dir),
    case filelib:is_dir(GraphDir) of
      true -> gs_graph_sup:start_child(list_to_binary(Dir), GraphDir);
      false -> ok
    end
  end,
  lists:foreach(Load, List),
  {noreply, Ctx};
handle_info(_, Ctx) ->
  {noreply, Ctx}.

terminate(_Reason, _Ctx) ->
  ok.

code_change(_OldVsn, Ctx, _Extra) ->
  {ok, Ctx}.