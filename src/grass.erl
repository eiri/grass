  %% WorkDir = os:cmd("mktemp -d /tmp/grass.XXXXXX"),%% @doc Grass is a toy graph database with LevelBD as a backend.
%% @version 0.1
%% @reference <a href="https://github.com/eiri/grass">https://github.com/eiri/grass</a>
%% @author Vitaliy Avdeyev <eiri@eiri.ca>
%% @copyright 2012 Vitaliy Avdeyev
%% @todo Make it compatable with <a href="https://github.com/tinkerpop">Tinkerpop</a>

-module(grass).

-author('Vitaliy Avdeyev <eiri@eiri.ca>').

-behaviour(application).
-behaviour(supervisor).

-define(WORKER(M), {M, {M, start_link, []}, permanent, 2000, worker, [M]}).
-define(WORKER(M, A), {M, {M, start_link, [A]}, permanent, 2000, worker, [M]}).
-define(GRAPH, gs_graph_server).
-define(EDGE, " -- ").

-type vertex() :: binary().
-type error() :: not_found | already_exists | {eleveldb, binary()}.

%% pub API
-export([
  verticies/0,
  verticies/1,
  vertex_exists/1,
  add_vertex/1,
  clone_vertex/2,
  modify_vertex/2,
  del_vertex/1,
  edges/0,
  edges/1,
  edge_exists/2,
  add_edge/2,
  del_edge/2,
  drop/0,
  is_empty/0
]).
%% stats, example & pick-inside functions
-export([all/0, stats/0, example/1]).
%% behaviours callbacks
-export([start/0, stop/0, start/2, stop/1, init/1]).

%%
%% API
%%

%% @doc Returns a list of all verticies of the graph.
%%  If the graph is empty, returns an empty list.
-spec verticies() -> list().
verticies() ->
  gkeys().

%% @doc Returns a list of the verticies connected to the vertex V.
%%  If the vertex doesn't have the connections returns an empty list.
-spec verticies(vertex()) -> list() | {error, error()}.
verticies(V) ->
  case gget(V) of
    {ok, V, Ins} -> Ins;
    E -> E
  end.

%% @doc Checks is the vertex V exists.
-spec vertex_exists(vertex()) -> boolean().
vertex_exists(V) ->
  gexists(V).

%% @doc Creates a new vertex V
-spec add_vertex(vertex()) -> ok | {error, error()}.
add_vertex(V) ->
  case gexists(V) of
    true -> {error, already_exists};
    false -> gput(V, [])
  end.

%% @doc Creates an exact copy of the vertex V1 with name V2.
%%  Duplicates all the edges connecting to V1.
-spec clone_vertex(vertex(), vertex()) -> ok | {error, error()}.
clone_vertex(From, To) ->
  case gget(From) of
    {ok, From, Ins} ->
      ok = gput(To, []),
      [ ok = grass:add_edge(V, To) || V <- Ins ],
      ok;
    E -> E
  end.

%% @doc Convinience funtion. Creates an exact copy of the vertex V1 with name V2
%%  and deletes the vertex V1.
-spec modify_vertex(vertex(), vertex()) -> ok | {error, error()}.
modify_vertex(From, To) ->
  ok = grass:clone_vertex(From, To),
  ok = del_vertex(From).

%% @doc Deletes the vertex V.
-spec del_vertex(vertex()) -> ok | {error, error()}.
del_vertex(V) ->
  case gget(V) of
    {ok, V, Ins} ->
      lists:foreach(fun(V2) ->
        {ok, V2, Ins2} = gget(V2),
        ok = gput(V2, lists:delete(V, Ins2))
      end, Ins),
      gdel(V);
    E -> E
  end.

%% @doc Returns a list of all edges of the graph.
%%  If the graph is empty, returns an empty list.
-spec edges() -> list().
edges() ->
  All = gall(),
  Edges = lists:foldl(fun
            ({V1, Ins}, Acc) ->
              lists:foldl(fun(V2, A) ->
                case lists:member([V2, V1], A) of
                  true -> A;
                  false -> [[V1, V2]|A]
                end
              end, Acc, Ins)
          end, [], All),
  lists:reverse(Edges).

%% @doc Returns a list of the edges connecting to the vertex V
-spec edges(vertex()) -> list() | {error, error()}.
edges(V) ->
  case gget(V) of
    {ok, V, Ins} -> [ [V, V2] || V2 <- Ins];
    E -> E
  end.

%% @doc Checks is the edge between verticies 'From' and 'To' exists.
-spec edge_exists(vertex(), vertex()) -> boolean().
edge_exists(From, To) ->
  case {gget(From), gget(To)} of
    {{ok, From, Ins1}, {ok, To, Ins2}} ->
      lists:member(To, Ins1) and lists:member(From, Ins2);
    _ -> false
  end.

%% @doc Adds edge between verticies 'From' and 'To'.
%%  Returns 'ok' even if the edge already exists.
-spec add_edge(vertex(), vertex()) -> ok | {error, error()}.
add_edge(From, To) ->
  case {gget(From), gget(To)} of
    {{ok, From, Ins1}, {ok, To, Ins2}} ->
      ok = gput(From, lists:usort([To|Ins1])),
      ok = gput(To, lists:usort([From|Ins2]));
    {E, {ok, _, _}} -> E;
    {_, E} -> E
  end.

%% @doc Deletes edge between the verticies 'From' and 'To'.
%%  Returns 'ok' even if the edge doesn't exist.
-spec del_edge(vertex(), vertex()) -> ok | {error, error()}.
del_edge(From, To) ->
  case {gget(From), gget(To)} of
    {{ok, From, Ins1}, {ok, To, Ins2}} ->
      ok = gput(From, lists:delete(To, Ins1)),
      ok = gput(To, lists:delete(From, Ins2));
    {E, {ok, _, _}} -> E;
    {_, E} -> E
  end.

%% @doc Checks if the graph have ny verticies.
-spec is_empty() -> boolean().
is_empty() ->
  gen_server:call(?GRAPH, is_empty).

%% @doc Deletes all the verticies and edges from the graph.
%%   Kind of analog of 'drop table' in RDBMS.
-spec drop() -> ok | {error, error()}.
drop() ->
  gen_server:cast(?GRAPH, drop).

%% Private

gall() ->
  gen_server:call(?GRAPH, all).

gall(From) ->
  gen_server:call(?GRAPH, {all, From}).

gkeys() ->
  gen_server:call(?GRAPH, keys).

gkeys(From) ->
  gen_server:call(?GRAPH, {keys, From}).

gexists(K) ->
  gen_server:call(?GRAPH, {exists, K}).

gput(K, V) ->
  gen_server:cast(?GRAPH, {put, K, V}).

gget(K) ->
  gen_server:call(?GRAPH, {get, K}).

gdel(K) ->
  gen_server:cast(?GRAPH, {delete, K}).

%% Move to public API with multigraph

all() ->
  gen_server:call(?GRAPH, all).

stats() ->
  lager:info("~n~s", [gen_server:call(?GRAPH, stats)]).

example(tiger) ->
  example("tiger.txt");
example(limeric) ->
  example("limeric.txt");
example(jabberwocky) ->
  example("jabberwocky.txt");
example(File) ->
  DirBin = filename:dirname(code:which(?MODULE)),
  {ok, Bin} = file:read_file(filename:join([DirBin,"..", "priv", File])),
  Punct = [<<"\n">>, <<".">>, <<",">>, <<"?">>, <<"!">>, <<"\"">>, <<":">>, <<";">>, <<" ">>],
  Parts = binary:split(Bin, Punct, [global, trim]),
  Text = [ list_to_binary(xmerl_lib:to_lower(binary_to_list(W))) || W <- Parts, W /= <<>> ],
  grass:drop(),
  lists:foldl(fun
    (W, first) -> grass:add_vertex(W), W;
    (W, Prev) ->
      grass:add_vertex(W),
      grass:add_edge(Prev, W),
      W
  end, first, Text),
  done.

%% app/sup start/stop

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
  {ok, Port} = application:get_env(port),
  %% in dispatcher '*' is atom, even when first argument is a string!
  WebConfig = [{ip, "0.0.0.0"}, {port, Port}, {dispatch, [{['*'], gs_web, []}]}],
  Web = {web, {webmachine_mochiweb, start, [WebConfig]}, permanent, 2000, worker, dynamic},
  %% Split it to graph sup
  {ok, DBDir} = application:get_env(work_dir),
  BaseDir = filename:dirname(code:which(?MODULE)),
  WorkDir = filename:join([BaseDir, "..", DBDir]),
  file:make_dir(WorkDir),
  LevelDB = ?WORKER(gs_graph_server, [WorkDir]),
  Children = [Web, LevelDB],
  % strategy
  MaxRestart = 3,
  MaxWait = 3600,
  RestartStrategy = {one_for_one, MaxRestart, MaxWait},
  {ok, {RestartStrategy, Children}}.

%%
%% EUnit tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

grass_test_() ->
  {setup,
    fun test_setup/0,
    fun test_teardown/1,
    fun(_Pid) ->
      {inorder, [
        test_add_vertex(),
        test_vertex_exists(),
        test_verticies(),
        test_add_edge(),
        test_edges(),
        test_verticies_for_vertex(),
        test_edges_for_vertex(),
        test_edge_exists(),
        test_clone_vertex(),
        test_modify_vertex(),
        test_del_edge(),
        test_del_vertex(),
        test_drop_and_is_empty()
      ]}
    end
  }.

test_setup() ->
  WD = os:cmd("mktemp -d /tmp/grass.XXXXXX"),
  WorkDir = string:strip(WD, right, $\n),
%%  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])),
  ?debugFmt("Work Dir: ~p~n", [WorkDir]),
  {ok, Ref} = gs_graph_server:start_link([WorkDir]),
  [{dir, WorkDir}, {ref, Ref}].

test_teardown(TestData) ->
  WorkDir = proplists:get_value(dir, TestData),
  Ref = proplists:get_value(ref, TestData),
  gs_graph_server:stop(Ref),
  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])).

test_add_vertex() ->
  [
    {"Add vertex", ?_assertEqual(ok, grass:add_vertex(<<"a">>))},
    {"Error on duplicate", ?_assertEqual({error, already_exists}, grass:add_vertex(<<"a">>))}
  ].

test_vertex_exists() ->
  [
    {"True on existing vertex", ?_assert(grass:vertex_exists(<<"a">>))},
    {"False on missing vertex", ?_assertNot(grass:vertex_exists(<<"b">>))}
  ].

test_verticies() ->
  All = fun() ->
    grass:add_vertex(<<"b">>),
    grass:add_vertex(<<"c">>),
    grass:verticies()
  end,
  [
    {"List of all verticies", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], All())}
  ].

test_add_edge() ->
  [
    {"Add edge", ?_assertEqual(ok, grass:add_edge(<<"a">>, <<"b">>))},
    {"'ok' on adding an existing edge", ?_assertEqual(ok, grass:add_edge(<<"b">>, <<"a">>))}
  ].

test_edges() ->
  Edges = fun() ->
    grass:add_vertex(<<"d">>),
    grass:add_edge(<<"c">>, <<"b">>),
    grass:add_edge(<<"c">>, <<"d">>),
    grass:add_edge(<<"a">>, <<"c">>),
    grass:edges()
  end,
  [
    {"Small list of all edges", ?_assertEqual([[<<"a">>, <<"b">>]], grass:edges())},
    {"Full list of all edges", ?_assertEqual([[<<"a">>, <<"b">>], [<<"a">>, <<"c">>], [<<"b">>, <<"c">>], [<<"c">>, <<"d">>]], Edges())}
  ].

test_verticies_for_vertex() ->
  [
    {"Got verticies for 'a'", ?_assertEqual([<<"b">>, <<"c">>], grass:verticies(<<"a">>))},
    {"Got verticies for 'b'", ?_assertEqual([<<"a">>, <<"c">>], grass:verticies(<<"b">>))},
    {"Got verticies for 'c'", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(<<"c">>))},
    {"Got verticies for 'd'", ?_assertEqual([<<"c">>], grass:verticies(<<"d">>))}
  ].

test_edges_for_vertex() ->
  [
    {"Got edges for 'a'", ?_assertEqual([[<<"a">>, <<"b">>], [<<"a">>, <<"c">>]], grass:edges(<<"a">>))},
    {"Got edges for 'b'", ?_assertEqual([[<<"b">>, <<"a">>], [<<"b">>, <<"c">>]], grass:edges(<<"b">>))},
    {"Got edges for 'c'", ?_assertEqual([[<<"c">>, <<"a">>], [<<"c">>, <<"b">>], [<<"c">>, <<"d">>]], grass:edges(<<"c">>))},
    {"Got edges for 'd'", ?_assertEqual([[<<"d">>, <<"c">>]], grass:edges(<<"d">>))}
  ].

test_edge_exists() ->
  [
    {"Got 'true' for existing edge", ?_assert(grass:edge_exists(<<"a">>, <<"c">>))},
    {"Got 'false' for non existing edge", ?_assertNot(grass:edge_exists(<<"a">>, <<"d">>))}
  ].

test_clone_vertex() ->
  [
    {"Clone vertex", ?_assertEqual(ok, grass:clone_vertex(<<"c">>, <<"e">>))},
    {"Correct connections in cloned", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(<<"e">>))},
    {"Correct edges in cloned", ?_assertEqual([[<<"e">>, <<"a">>], [<<"e">>, <<"b">>], [<<"e">>, <<"d">>]], grass:edges(<<"e">>))},
    {"'e' knows 'a'", ?_assert(grass:edge_exists(<<"e">>, <<"a">>))},
    {"'a' knows 'e'", ?_assert(grass:edge_exists(<<"a">>, <<"e">>))}
  ].

test_modify_vertex() ->
  [
    {"Modify vertex", ?_assertEqual(ok, grass:modify_vertex(<<"e">>, <<"eh">>))},
    {"'e' no more", ?_assertNot(grass:vertex_exists(<<"e">>))},
    {"Correct connections in modified", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(<<"eh">>))},
    {"Correct edges in modified", ?_assertEqual([[<<"eh">>, <<"a">>], [<<"eh">>, <<"b">>], [<<"eh">>, <<"d">>]], grass:edges(<<"eh">>))},
    {"'eh' knows 'a'", ?_assert(grass:edge_exists(<<"eh">>, <<"a">>))},
    {"'a' knows 'eh'", ?_assert(grass:edge_exists(<<"a">>, <<"eh">>))},
    {"'eh' knows 'b'", ?_assert(grass:edge_exists(<<"eh">>, <<"b">>))},
    {"'b' knows 'eh'", ?_assert(grass:edge_exists(<<"b">>, <<"eh">>))},
    {"'eh' knows 'd'", ?_assert(grass:edge_exists(<<"eh">>, <<"d">>))},
    {"'d' knows 'eh'", ?_assert(grass:edge_exists(<<"d">>, <<"eh">>))}
  ].

test_del_edge() ->
  [
    {"Delete existing edge", ?_assertEqual(ok, grass:del_edge(<<"eh">>, <<"d">>))},
    {"Delete non existing edge", ?_assertEqual(ok, grass:del_edge(<<"a">>, <<"d">>))},
    {"Deleted edge gone", ?_assertNot(grass:edge_exists(<<"eh">>, <<"d">>))},
    {"Non delete edge to 'a' preserved", ?_assert(grass:edge_exists(<<"eh">>, <<"a">>))},
    {"Non delete edge to 'b' preserved", ?_assert(grass:edge_exists(<<"b">>, <<"eh">>))}
  ].

test_del_vertex() ->
  [
    {"Delete vertex 'eh'", ?_assertEqual(ok, grass:del_vertex(<<"eh">>))},
    {"All edges of 'eh' gone", ?_assertEqual([[<<"a">>, <<"b">>], [<<"a">>, <<"c">>], [<<"b">>, <<"c">>], [<<"c">>, <<"d">>]], grass:edges())},
    {"All verticies preserved", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass:verticies())}
  ].

test_drop_and_is_empty() ->
  [
    {"Drop", ?_assertEqual(ok, grass:drop())},
    {"IsEmpty", ?_assert(grass:is_empty())},
    {"It is really empty", ?_assertEqual([], grass:verticies())}
  ].

-endif.