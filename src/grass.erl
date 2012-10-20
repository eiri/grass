-module(grass).

-author('Vitaliy Avdeyev <eiri@eiri.ca>').

-behaviour(application).
-behaviour(supervisor).

-define(WORKER(M), {M, {M, start_link, []}, permanent, 2000, worker, [M]}).
-define(WORKER(M, A), {M, {M, start_link, [A]}, permanent, 2000, worker, [M]}).
-define(STORAGE, gs_leveldb_server).

%% pub API
-export([start/0, stop/0, stats/0, bootstrap/0, all/0, get/1, put/2, delete/1, drop/0, is_empty/0]).
%% behaviours callbacks
-export([start/2, stop/1, init/1]).

%% API

stats() ->
  lager:info("~n~s", [gen_server:call(?STORAGE, stats)]).

bootstrap() ->
  Add = fun(A, B) ->
    Key = <<A/binary, B/binary>>,
    NA = neighbours(A),
    NB = neighbours(B),
    Neighbours = lists:usort(lists:append(NA, NB)),
    Value = [{<<"all">>, Neighbours},{<<"first">>, NA}, {<<"second">>, NB}],
    gen_server:cast(?STORAGE, {put, Key, Value})
  end,
  AZ = [ atom_to_binary(A, latin1) || A <- [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] ],
  [ Add(A, B) || A <- AZ, B <- AZ ].

neighbours(Letter) ->
  AZ = [ atom_to_binary(A, latin1) || A <- [a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z] ],
  List = lists:zip(AZ, lists:seq(1, length(AZ))),
  case proplists:get_value(Letter, List) of
    1 -> [lists:nth(26, AZ), lists:nth(2, AZ)];
    26 -> [lists:nth(25, AZ), lists:nth(1, AZ)];
    N -> [lists:nth(N - 1, AZ), lists:nth(N + 1, AZ)]
  end.

all() ->
  gen_server:call(?STORAGE, all).

get(Key) ->
  gen_server:call(?STORAGE, {get, atom_to_binary(Key, latin1)}).

put(Key, Value) ->
  gen_server:cast(?STORAGE,
      {put, atom_to_binary(Key, latin1), atom_to_binary(Value, latin1)}).

delete(Key) ->
  gen_server:cast(?STORAGE, {delete, atom_to_binary(Key, latin1)}).

is_empty() ->
  gen_server:call(?STORAGE, is_empty).

drop() ->
  gen_server:cast(?STORAGE, drop).

%% start/stop

start() ->
  Fmt = "* Starting ~p: [~p]~n",
  Deps = [lager, crypto, inets, mochiweb, webmachine, grass],
  [ error_logger:info_msg(Fmt, [App, application:start(App)]) || App <- Deps ].

stop() ->
  Fmt = "* Stopping ~p: [~p]~n",
  Deps = [lager, crypto, inets, mochiweb, webmachine, grass],
  [ error_logger:info_msg(Fmt, [App, application:stop(App)]) || App <- lists:reverse(Deps) ].

start(_Type, _StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

init([]) ->
  Port = 9922,
  MaxRestart = 3,
  MaxWait = 3600,
  RestartStrategy = {one_for_one, MaxRestart, MaxWait},
  %% in dispatcher '*' is atom, even when first argument is a string!
  WebConfig = [{ip, "0.0.0.0"}, {port, Port}, {dispatch, [{['*'], gs_web, []}]}],
  Web = {web, {webmachine_mochiweb, start, [WebConfig]}, permanent, 2000, worker, dynamic},
  WorkDir = "test.db", %% FIXME! move to app.config
  LevelDB = ?WORKER(gs_leveldb_server, [WorkDir]),
  Children = [Web, LevelDB],
  {ok, {RestartStrategy, Children}}.
