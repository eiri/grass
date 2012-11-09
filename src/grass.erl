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
-define(GRAPH, gs_leveldb_server).
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
%% stats & pick-inside functions
-export([all/0, stats/0]).
%% behaviours callbacks
-export([start/0, stop/0, start/2, stop/1, init/1]).

%%
%% API
%%

%% @doc Returns a list of all verticies of the graph.
%%  If the graph is empty, returns an empty list.
-spec verticies() -> list().
verticies() ->
  Keys = gen_server:call(?GRAPH, {keys, <<"vertex">>}),
  lists:foldl(fun
    (<<"vertex/", V/binary>>, Acc) -> [V|Acc];
    (_, Acc) -> Acc
  end,
  [], lists:reverse(Keys)).

%% @doc Returns a list of the verticies connected to the vertex V.
%%  If the vertex doesn't have the connections returns an empty list.
-spec verticies(vertex()) -> list() | {error, error()}.
verticies(V) ->
  V1 = <<"vertex/", V/binary>>,
  case gget(V1) of
    {ok, V1, Ins} -> Ins;
    E -> E
  end.

%% @doc Checks is the vertex V exists.
-spec vertex_exists(vertex()) -> boolean().
vertex_exists(V) ->
  gexists(<<"vertex/", V/binary>>).

%% @doc Creates a new vertex V
-spec add_vertex(vertex()) -> ok | {error, error()}.
add_vertex(V) ->
  V1 = <<"vertex/", V/binary>>,
  case gexists(V1) of
    true -> {error, already_exists};
    false -> gput(V1, [])
  end.

%% @doc Creates an exact copy of the vertex V1 with name V2.
%%  Duplicates all the edges connecting to V1.
-spec clone_vertex(vertex(), vertex()) -> ok | {error, error()}.
clone_vertex(From, To) ->
  V1 = <<"vertex/", From/binary>>,
  case gget(V1) of
    {ok, V1, Ins} ->
      ok = gput(<<"vertex/", To/binary>>, []),
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
  V1 = <<"vertex/", V/binary>>,
  case gget(V1) of
    {ok, V1, Ins} ->
      lists:foreach(fun(RawV) ->
        V2 = <<"vertex/", RawV/binary>>,
        {ok, V2, Ins2} = gget(V2),
        ok = gput(V2, lists:delete(V, Ins2))
      end, Ins),
      gdel(V1);
    E -> E
  end.

%% @doc Returns a list of all edges of the graph.
%%  If the graph is empty, returns an empty list.
-spec edges() -> list().
edges() ->
  All = gen_server:call(?GRAPH, {all, <<"vertex">>}),
  Edges = lists:foldl(fun
            ({<<"vertex/", V1/binary>>, Ins}, Acc) ->
              lists:foldl(fun(V2, A) ->
                case lists:member(<<V2/binary, ?EDGE, V1/binary>>, A) of
                  true -> A;
                  false -> [<<V1/binary, ?EDGE, V2/binary>>|A]
                end
              end, Acc, Ins);
            (_, Acc) -> Acc
          end, [], All),
  lists:reverse(Edges).

%% @doc Returns a list of the edges connecting to the vertex V
-spec edges(vertex()) -> list() | {error, error()}.
edges(V) ->
  V1 = <<"vertex/", V/binary>>,
  case gget(V1) of
    {ok, V1, Ins} -> [ <<V/binary, ?EDGE, V2/binary>> || V2 <- Ins];
    E -> E
  end.

%% @doc Checks is the edge between verticies V1 and V2 exists.
-spec edge_exists(vertex(), vertex()) -> boolean().
edge_exists(V1, V2) ->
  Vr1 = <<"vertex/", V1/binary>>,
  Vr2 = <<"vertex/", V2/binary>>,
  case {gget(Vr1), gget(Vr2)} of
    {{ok, Vr1, Ins1}, {ok, Vr2, Ins2}} ->
      lists:member(V2, Ins1) and lists:member(V1, Ins2);
    _ -> false
  end.

%% @doc Adds edge between verticies 'From' and 'To'.
%%  Returns 'ok' even if the edge already exists.
-spec add_edge(vertex(), vertex()) -> ok | {error, error()}.
add_edge(From, To) ->
  From1 = <<"vertex/", From/binary>>,
  To1 = <<"vertex/", To/binary>>,
  case {gget(From1), gget(To1)} of
    {{ok, From1, Ins1}, {ok, To1, Ins2}} ->
      ok = gput(From1, lists:usort([To|Ins1])),
      ok = gput(To1, lists:usort([From|Ins2]));
    {E, {ok, _, _}} -> E;
    {_, E} -> E
  end.

%% @doc Deletes edge between the verticies V1 and V2.
%%  Returns 'ok' even if the edge doesn't exist.
-spec del_edge(vertex(), vertex()) -> ok | {error, error()}.
del_edge(From, To) ->
  From1 = <<"vertex/", From/binary>>,
  To1 = <<"vertex/", To/binary>>,
  case {gget(From1), gget(To1)} of
    {{ok, From1, Ins1}, {ok, To1, Ins2}} ->
      ok = gput(From1, lists:delete(To, Ins1)),
      ok = gput(To1, lists:delete(From, Ins2));
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
  {ok, WorkDir} = application:get_env(work_dir),
  MaxRestart = 3,
  MaxWait = 3600,
  RestartStrategy = {one_for_one, MaxRestart, MaxWait},
  %% in dispatcher '*' is atom, even when first argument is a string!
  WebConfig = [{ip, "0.0.0.0"}, {port, Port}, {dispatch, [{['*'], gs_web, []}]}],
  Web = {web, {webmachine_mochiweb, start, [WebConfig]}, permanent, 2000, worker, dynamic},
  LevelDB = ?WORKER(gs_leveldb_server, [WorkDir]),
  Children = [Web, LevelDB],
  {ok, {RestartStrategy, Children}}.

%%
%% EUnit tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

verticies_test_() ->
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
  {ok, Ref} = gs_leveldb_server:start_link([WorkDir]),
  [{dir, WorkDir}, {ref, Ref}].

test_teardown(TestData) ->
  WorkDir = proplists:get_value(dir, TestData),
  Ref = proplists:get_value(ref, TestData),
  gs_leveldb_server:stop(Ref),
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
    {"Small list of all edges", ?_assertEqual([<<"a -- b">>], grass:edges())},
    {"Full list of all edges", ?_assertEqual([<<"a -- b">>, <<"a -- c">>, <<"b -- c">>, <<"c -- d">>], Edges())}
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
    {"Got edges for 'a'", ?_assertEqual([<<"a -- b">>, <<"a -- c">>], grass:edges(<<"a">>))},
    {"Got edges for 'b'", ?_assertEqual([<<"b -- a">>, <<"b -- c">>], grass:edges(<<"b">>))},
    {"Got edges for 'c'", ?_assertEqual([<<"c -- a">>, <<"c -- b">>, <<"c -- d">>], grass:edges(<<"c">>))},
    {"Got edges for 'd'", ?_assertEqual([<<"d -- c">>], grass:edges(<<"d">>))}
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
    {"Correct edges in cloned", ?_assertEqual([<<"e -- a">>, <<"e -- b">>, <<"e -- d">>], grass:edges(<<"e">>))},
    {"'e' knows 'a'", ?_assert(grass:edge_exists(<<"e">>, <<"a">>))},
    {"'a' knows 'e'", ?_assert(grass:edge_exists(<<"a">>, <<"e">>))}
  ].

test_modify_vertex() ->
  [
    {"Modify vertex", ?_assertEqual(ok, grass:modify_vertex(<<"e">>, <<"eh">>))},
    {"'e' no more", ?_assertNot(grass:vertex_exists(<<"e">>))},
    {"Correct connections in modified", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(<<"eh">>))},
    {"Correct edges in modified", ?_assertEqual([<<"eh -- a">>, <<"eh -- b">>, <<"eh -- d">>], grass:edges(<<"eh">>))},
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
    {"All edges of 'eh' gone", ?_assertEqual([<<"a -- b">>, <<"a -- c">>, <<"b -- c">>, <<"c -- d">>], grass:edges())},
    {"All verticies preserved", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass:verticies())}
  ].

test_drop_and_is_empty() ->
  [
    {"Drop", ?_assertEqual(ok, grass:drop())},
    {"IsEmpty", ?_assert(grass:is_empty())},
    {"It is really empty", ?_assertEqual([], grass:verticies())}
  ].

-endif.