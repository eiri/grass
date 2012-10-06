-module(grass).

-author('Vitaliy Avdeyev <eiri@eiri.ca>').

-behaviour(application).
-behaviour(supervisor).

-define(WORKER(M, A), {M, {M, start_link, [A]}, permanent, 2000, worker, [M]}).

%% pub API
-export([start/0, stop/0]).
%% behaviours callbacks
-export([start/2, stop/1, init/1]).

%% start/stop

start() ->
  Fmt = "* Starting ~p: [~p]~n",
  Deps = [lager, crypto, inets, mochiweb, webmachine, grass],
  [ io:format(Fmt, [App, application:start(App)]) || App <- Deps ].

stop() ->
  Fmt = "* Stopping ~p: [~p]~n",
  Deps = [lager, crypto, inets, mochiweb, webmachine, grass],
  [ io:format(Fmt, [App, application:stop(App)]) || App <- lists:reverse(Deps) ].

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
  LevelDB = ?WORKER(gs_leveldb_server, []),
  Children = [Web, LevelDB],
  {ok, {RestartStrategy, Children}}.
