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

-define(SUPER(M), {M, {M, start_link, []}, permanent, 5000, supervisor, [M]}).
-define(WORKER(M, A), {M, {M, start_link, [A]}, permanent, 2000, worker, [M]}).
-define(EDGE, " -- ").

-type graph() :: binary() | pid().
-type vertex() :: binary().
-type key() :: binary().
-type value() :: term().
-type tag() :: {key(), value()}.
-type error() :: not_found | already_exists | {eleveldb, binary()}.

%% pub API
-export([
  graphs/0,
  create/1,
  verticies/1,
  verticies/2,
  vertex_exists/2,
  add_vertex/2,
  clone_vertex/3,
  modify_vertex/3,
  del_vertex/2,
  edges/1,
  edges/2,
  edge_exists/3,
  add_edge/3,
  del_edge/3,
  tags/2,
  tags/3,
  add_tag/4,
  del_tag/3,
  drop/1,
  destroy/1,
  is_empty/1
]).
%% stats, example & pick-inside functions
-export([stats/1, example/1]).
%% behaviours callbacks
-export([start/0, stop/0, start/2, stop/1, init/1]).

%%
%% API
%%

%% @doc Shows list of available graphs.
-spec graphs() -> [graph(),...] | [].
graphs() ->
  [ G || {G,_} <- gs_register_server:get() ].

%% @doc Creates a new graph G.
-spec create(graph()) -> ok | error().
create(G) ->
  gs_register_server:create(G).

%% @doc Deletes the graph G.
%%  This function not only drops all the verticies on the graph, but also 
%%  removes graph's directory and shutting down graph's server.
-spec destroy(graph()) -> ok | error().
destroy(G) ->
  gs_register_server:destroy(G).

%% @doc Returns a list of all verticies of the graph G.
%%  If the graph is empty, returns an empty list.
-spec verticies(graph()) -> list() | error().
verticies(G) ->
  gkeys(G).

%% @doc Returns a list of the verticies of graph G connected to the vertex V.
%%  If the vertex doesn't have the connections returns an empty list.
-spec verticies(graph(), vertex()) -> list() | {error, error()}.
verticies(G, V) ->
  case gget(G, V) of
    {ok, V, Ins} -> Ins;
    E -> E
  end.

%% @doc Checks is the vertex V exists in graph G.
-spec vertex_exists(graph(), vertex()) -> boolean().
vertex_exists(G, V) ->
  gexists(G, V).

%% @doc Creates a new vertex V in graph G
-spec add_vertex(graph(), vertex()) -> ok | {error, error()}.
add_vertex(G, V) ->
  case gexists(G, V) of
    true -> {error, already_exists};
    false -> gput(G, V, [])
  end.

%% @doc Creates an exact copy of the vertex V1 with name V2 in graph G.
%%  Duplicates all the edges connecting to V1.
-spec clone_vertex(graph(), vertex(), vertex()) -> ok | {error, error()}.
clone_vertex(G, From, To) ->
  case gget(G, From) of
    {ok, From, Ins} ->
      ok = gput(G, To, []),
      [ ok = grass:add_edge(G, V, To) || V <- Ins ],
      ok;
    E -> E
  end.

%% @doc Convinience funtion. Creates an exact copy of the vertex V1 with name V2 in graph G.
%%  and deletes the vertex V1.
-spec modify_vertex(graph(), vertex(), vertex()) -> ok | {error, error()}.
modify_vertex(G, From, To) ->
  ok = grass:clone_vertex(G, From, To),
  ok = del_vertex(G, From).

%% @doc Deletes the vertex V in graph G.
-spec del_vertex(graph(), vertex()) -> ok | {error, error()}.
del_vertex(G, V) ->
  case gget(G, V) of
    {ok, V, Ins} ->
      lists:foreach(fun(V2) ->
        {ok, V2, Ins2} = gget(G, V2),
        ok = gput(G, V2, lists:delete(V, Ins2))
      end, Ins),
      gdel(G, V);
    E -> E
  end.

%% @doc Returns a list of all edges of the graph G.
%%  If the graph is empty, returns an empty list.
-spec edges(graph()) -> list().
edges(G) ->
  case gall(G) of
    {error, E} -> {error, E};
    All ->
      Edges = lists:foldl(fun
            ({V1, Ins}, Acc) ->
              lists:foldl(fun(V2, A) ->
                case lists:member([V2, V1], A) of
                  true -> A;
                  false -> [[V1, V2]|A]
                end
              end, Acc, Ins)
          end, [], All),
      lists:reverse(Edges)
  end.

%% @doc Returns a list of the edges connecting to the vertex V in graph G.
-spec edges(graph(), vertex()) -> list() | {error, error()}.
edges(G, V) ->
  case gget(G, V) of
    {ok, V, Ins} -> [ [V, V2] || V2 <- Ins];
    E -> E
  end.

%% @doc Checks if an edge between verticies 'From' and 'To' exists in graph G.
-spec edge_exists(graph(), vertex(), vertex()) -> boolean().
edge_exists(G, From, To) ->
  case {gget(G, From), gget(G, To)} of
    {{ok, From, Ins1}, {ok, To, Ins2}} ->
      lists:member(To, Ins1) and lists:member(From, Ins2);
    _ -> false
  end.

%% @doc Adds edge between verticies 'From' and 'To' in graph G.
%%  Returns 'ok' even if the edge already exists.
-spec add_edge(graph(), vertex(), vertex()) -> ok | {error, error()}.
add_edge(G, From, To) ->
  case {gget(G, From), gget(G, To)} of
    {{ok, From, Ins1}, {ok, To, Ins2}} ->
      ok = gput(G, From, lists:usort([To|Ins1])),
      ok = gput(G, To, lists:usort([From|Ins2]));
    {E, {ok, _, _}} -> E;
    {_, E} -> E
  end.

%% @doc Deletes the edge between the verticies 'From' and 'To' in graph G.
%%  Returns 'ok' even if the edge doesn't exist.
-spec del_edge(graph(), vertex(), vertex()) -> ok | {error, error()}.
del_edge(G, From, To) ->
  case {gget(G, From), gget(G, To)} of
    {{ok, From, Ins1}, {ok, To, Ins2}} ->
      ok = gput(G, From, lists:delete(To, Ins1)),
      ok = gput(G, To, lists:delete(From, Ins2));
    {E, {ok, _, _}} -> E;
    {_, E} -> E
  end.

%% @doc Gets all the attributes that belong to a given vertex V in graph G.
-spec tags(graph(), vertex()) -> [tag(),...] | [] | {error, error()}.
tags(G, V) ->
  case gall(G, tags, V) of
    {error, E} -> {error, E};
    All ->
      Fold = fun({PfxKey, Val}, Acc) ->
        case binary:split(PfxKey, <<"/">>) of
          [V, Key] -> [{Key, Val}] ++ Acc;
          _ -> Acc
        end
      end,
      lists:reverse(lists:foldl(Fold, [], All))
  end.

%% @doc Gets the value of the attribute K in vertex V of graph G
%%  Return undefined if there is no such attribute.
-spec tags(graph(), vertex(), key()) -> {key(), value()} | undefined | {error, error()}.
tags(G, V, Key) ->
  case gexists(G, V) of
    false -> {error, not_found};
    true ->
      PfxKey = <<V/binary, "/", Key/binary>>,
      case gget(G, tags, PfxKey) of
        {ok, PfxKey, Val} -> {Key, Val};
        Err -> Err
      end
  end.

%% @doc Adds the attribute to a vertex V in graph G.
%%  If the attribute already exists, updates it.
-spec add_tag(graph(), vertex(), key(), value()) -> ok | {error, error()}.
add_tag(G, V, Key, Val) ->
  case gexists(G, V) of
    false -> {error, not_found};
    true ->
      PfxKey = <<V/binary, "/", Key/binary>>,
      gput(G, tags, PfxKey, Val)
  end.

%% @doc Removes the attribute with key K from vertex V in graph G.
%%  If the attribute doesn't exists just returns ok.
-spec del_tag(graph(), vertex(), key()) -> ok | {error, error()}.
del_tag(G, V, Key) ->
  PfxKey = <<V/binary, "/", Key/binary>>,
  gdel(G, tags, PfxKey).

%% @doc Checks if the graph G have any verticies.
-spec is_empty(graph()) -> boolean().
is_empty(G) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> grass:is_empty(Pid);
    E -> E
  end;
is_empty(G) ->
  gen_server:call(G, is_empty).

%% @doc Deletes all the verticies and edges in the graph G.
%%   Kind of like ab analog of 'drop table' in RDBMS.
-spec drop(graph()) -> ok | {error, error()}.
drop(G) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> grass:drop(Pid);
    E -> E
  end;
drop(G) ->
  gen_server:call(G, drop).

%% Private
gall(G) ->
  gall(G, verticies).

gall(G, DB) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gall(Pid, DB);
    E -> E
  end;
gall(G, DB) ->
  gen_server:call(G, {all, DB}).

gall(G, DB, From) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gall(Pid, DB, From);
    E -> E
  end;
gall(G, DB, From) ->
  gen_server:call(G, {all, DB, From}).

gkeys(G) ->
  gkeys(G, verticies).

gkeys(G, DB) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gkeys(Pid, DB);
    E -> E
  end;
gkeys(G, DB) ->
  gen_server:call(G, {keys, DB}).

gexists(G, K) ->
  gexists(G, verticies, K).

gexists(G, DB, K) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gexists(Pid, DB, K);
    E -> E
  end;
gexists(G, DB, K) ->
  gen_server:call(G, {exists, DB, K}).

gput(G, K, V) ->
  gput(G, verticies, K, V).

gput(G, DB, K, V) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gput(Pid, DB, K, V);
    E -> E
  end;
gput(G, DB, K, V) ->
  gen_server:cast(G, {put, DB, K, V}).

gget(G, K) ->
  gget(G, verticies, K).

gget(G, DB, K) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gget(Pid, DB, K);
    E -> E
  end;
gget(G, DB, K) ->
  gen_server:call(G, {get, DB, K}).

gdel(G, K) ->
  gdel(G, verticies, K).

gdel(G, DB, K) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> gdel(Pid, DB, K);
    E -> E
  end;
gdel(G, DB, K) ->
  gen_server:cast(G, {delete, DB, K}).

%% Move to public API with multigraph

stats(G) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> grass:stats(Pid);
    E -> E
  end;
stats(G) ->
  Stats = gen_server:call(G, stats),
  VStats = proplists:get_value(verticies, Stats),
  TStats = proplists:get_value(tags, Stats),
  lager:info("~n-= Verticies =-~n~s~n-= Attributes =-~n~s", [VStats, TStats]).

example(tiger) ->
  example(<<"tiger">>, "tiger.txt");
example(limeric) ->
  example(<<"limeric">>, "limeric.txt");
example(jabberwocky) ->
  example(<<"jabberwocky">>, "jabberwocky.txt").

example(G, File) ->
  DirBin = filename:dirname(code:which(?MODULE)),
  {ok, Bin} = file:read_file(filename:join([DirBin,"..", "priv", File])),
  Color = fun(<<F,_/binary>>) ->
    case lists:member(F, [97, 101, 105, 111, 117]) of
      true -> <<"red">>;
      false -> <<"green">>
    end
  end,
  Punct = [<<"\n">>, <<".">>, <<",">>, <<"?">>, <<"!">>, <<"\"">>, <<":">>, <<";">>, <<" ">>],
  Parts = binary:split(Bin, Punct, [global, trim]),
  Text = [ list_to_binary(xmerl_lib:to_lower(binary_to_list(W))) || W <- Parts, W /= <<>> ],
  %% grass:destroy(G),
  grass:create(G),
  lists:foldl(fun
    (W, first) -> grass:add_vertex(G, W), W;
    (W, Prev) ->
      grass:add_vertex(G, W),
      grass:add_tag(G, W, <<"color">>, Color(W)),
      grass:add_tag(G, W, <<"sides">>, erlang:size(W)),
      grass:add_edge(G, Prev, W),
      W
  end, first, Text),
  done.

%% app/sup start/stop

start() ->
  Deps = [lager, crypto, inets, mochiweb, webmachine, grass],
  [ application:start(App) || App <- Deps ].

stop() ->
  Deps = [lager, crypto, inets, mochiweb, webmachine, grass],
  [ application:stop(App) || App <- lists:reverse(Deps) ].

start(_Type, _StartArgs) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop(_State) ->
  ok.

init([]) ->
  %% web (in dispatcher '*' is atom, even when first argument is a string!)
  {ok, Port} = application:get_env(port),
  WebConfig = [{ip, "0.0.0.0"}, {port, Port}, {dispatch, [{['*'], gs_web, []}]}],
  Web = {web, {webmachine_mochiweb, start, [WebConfig]}, permanent, 2000, worker, dynamic},
  %% graph
  {ok, DBDir} = application:get_env(work_dir),
  BaseDir = filename:dirname(code:which(?MODULE)),
  WorkDir = filename:join([BaseDir, "..", DBDir]),
  file:make_dir(WorkDir),
  Tid = ets:new(gs_register_server, [public]),
  Register = ?WORKER(gs_register_server, [Tid, WorkDir]),
  GraphSup = ?SUPER(gs_graph_sup),
  % strategy
  MaxRestart = 3,
  MaxWait = 3600,
  RestartStrategy = {one_for_one, MaxRestart, MaxWait},
  Children = [Web, GraphSup, Register],
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
    fun(Data) ->
      Pid = proplists:get_value(ref, Data),
      {inorder, [
        test_add_vertex(Pid),
        test_vertex_exists(Pid),
        test_verticies(Pid),
        test_tags(Pid),
        test_add_edge(Pid),
        test_edges(Pid),
        test_verticies_for_vertex(Pid),
        test_edges_for_vertex(Pid),
        test_edge_exists(Pid),
        test_clone_vertex(Pid),
        test_modify_vertex(Pid),
        test_del_edge(Pid),
        test_del_vertex(Pid),
        test_drop_and_is_empty(Pid)
      ]}
    end
  }.

test_setup() ->
  WD = os:cmd("mktemp -d /tmp/grass.XXXXXX"),
  WorkDir = string:strip(WD, right, $\n),
%%  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])),
  ?debugFmt("Work Dir: ~p~n", [WorkDir]),
  {ok, Ref} = gs_graph_server:start_link([{<<"test">>, WorkDir}]),
  [{dir, WorkDir}, {ref, Ref}].

test_teardown(TestData) ->
  WorkDir = proplists:get_value(dir, TestData),
  Ref = proplists:get_value(ref, TestData),
  gs_graph_server:stop(Ref),
  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])).

test_add_vertex(G) ->
  [
    {"Add vertex", ?_assertEqual(ok, grass:add_vertex(G, <<"a">>))},
    {"Error on duplicate", ?_assertEqual({error, already_exists}, grass:add_vertex(G, <<"a">>))}
  ].

test_vertex_exists(G) ->
  [
    {"True on existing vertex", ?_assert(grass:vertex_exists(G, <<"a">>))},
    {"False on missing vertex", ?_assertNot(grass:vertex_exists(G, <<"b">>))}
  ].

test_verticies(G) ->
  All = fun() ->
    grass:add_vertex(G, <<"b">>),
    grass:add_vertex(G, <<"c">>),
    grass:verticies(G)
  end,
  [
    {"List of all verticies", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>], All())}
  ].

test_tags(G) ->
  [
    {"Empty list of tags", ?_assertEqual([], grass:tags(G, <<"b">>))},
    {"Can add tag", ?_assertEqual(ok, grass:add_tag(G, <<"b">>, <<"color">>, <<"red">>))},
    {"Can add another tag", ?_assertEqual(ok, grass:add_tag(G, <<"b">>, <<"mood">>, <<"blue">>))},
    {"Can add tag to different vertex", ?_assertEqual(ok, grass:add_tag(G, <<"c">>, <<"color">>, <<"green">>))},
    {"Can get all tags", ?_assertEqual([{<<"color">>, <<"red">>}, {<<"mood">>, <<"blue">>}], grass:tags(G, <<"b">>))},
    {"Can get one tag", ?_assertEqual({<<"color">>, <<"red">>}, grass:tags(G, <<"b">>, <<"color">>))},
    {"Can delete tag", ?_assertEqual(ok, grass:del_tag(G, <<"b">>, <<"color">>))},
    {"Proper tag deleted", ?_assertEqual([{<<"mood">>, <<"blue">>}], grass:tags(G, <<"b">>))},
    {"No tag on other vertex affected", ?_assertEqual([{<<"color">>, <<"green">>}], grass:tags(G, <<"c">>))}
  ].

test_add_edge(G) ->
  [
    {"Add edge", ?_assertEqual(ok, grass:add_edge(G, <<"a">>, <<"b">>))},
    {"'ok' on adding an existing edge", ?_assertEqual(ok, grass:add_edge(G, <<"b">>, <<"a">>))}
  ].

test_edges(G) ->
  Edges = fun() ->
    grass:add_vertex(G, <<"d">>),
    grass:add_edge(G, <<"c">>, <<"b">>),
    grass:add_edge(G, <<"c">>, <<"d">>),
    grass:add_edge(G, <<"a">>, <<"c">>),
    grass:edges(G)
  end,
  [
    {"Small list of all edges", ?_assertEqual([[<<"a">>, <<"b">>]], grass:edges(G))},
    {"Full list of all edges", ?_assertEqual([[<<"a">>, <<"b">>], [<<"a">>, <<"c">>], [<<"b">>, <<"c">>], [<<"c">>, <<"d">>]], Edges())}
  ].

test_verticies_for_vertex(G) ->
  [
    {"Got verticies for 'a'", ?_assertEqual([<<"b">>, <<"c">>], grass:verticies(G, <<"a">>))},
    {"Got verticies for 'b'", ?_assertEqual([<<"a">>, <<"c">>], grass:verticies(G, <<"b">>))},
    {"Got verticies for 'c'", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(G, <<"c">>))},
    {"Got verticies for 'd'", ?_assertEqual([<<"c">>], grass:verticies(G, <<"d">>))}
  ].

test_edges_for_vertex(G) ->
  [
    {"Got edges for 'a'", ?_assertEqual([[<<"a">>, <<"b">>], [<<"a">>, <<"c">>]], grass:edges(G, <<"a">>))},
    {"Got edges for 'b'", ?_assertEqual([[<<"b">>, <<"a">>], [<<"b">>, <<"c">>]], grass:edges(G, <<"b">>))},
    {"Got edges for 'c'", ?_assertEqual([[<<"c">>, <<"a">>], [<<"c">>, <<"b">>], [<<"c">>, <<"d">>]], grass:edges(G, <<"c">>))},
    {"Got edges for 'd'", ?_assertEqual([[<<"d">>, <<"c">>]], grass:edges(G, <<"d">>))}
  ].

test_edge_exists(G) ->
  [
    {"Got 'true' for existing edge", ?_assert(grass:edge_exists(G, <<"a">>, <<"c">>))},
    {"Got 'false' for non existing edge", ?_assertNot(grass:edge_exists(G, <<"a">>, <<"d">>))}
  ].

test_clone_vertex(G) ->
  [
    {"Clone vertex", ?_assertEqual(ok, grass:clone_vertex(G, <<"c">>, <<"e">>))},
    {"Correct connections in cloned", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(G, <<"e">>))},
    {"Correct edges in cloned", ?_assertEqual([[<<"e">>, <<"a">>], [<<"e">>, <<"b">>], [<<"e">>, <<"d">>]], grass:edges(G, <<"e">>))},
    {"'e' knows 'a'", ?_assert(grass:edge_exists(G, <<"e">>, <<"a">>))},
    {"'a' knows 'e'", ?_assert(grass:edge_exists(G, <<"a">>, <<"e">>))}
  ].

test_modify_vertex(G) ->
  [
    {"Modify vertex", ?_assertEqual(ok, grass:modify_vertex(G, <<"e">>, <<"eh">>))},
    {"'e' no more", ?_assertNot(grass:vertex_exists(G, <<"e">>))},
    {"Correct connections in modified", ?_assertEqual([<<"a">>, <<"b">>, <<"d">>], grass:verticies(G, <<"eh">>))},
    {"Correct edges in modified", ?_assertEqual([[<<"eh">>, <<"a">>], [<<"eh">>, <<"b">>], [<<"eh">>, <<"d">>]], grass:edges(G, <<"eh">>))},
    {"'eh' knows 'a'", ?_assert(grass:edge_exists(G, <<"eh">>, <<"a">>))},
    {"'a' knows 'eh'", ?_assert(grass:edge_exists(G, <<"a">>, <<"eh">>))},
    {"'eh' knows 'b'", ?_assert(grass:edge_exists(G, <<"eh">>, <<"b">>))},
    {"'b' knows 'eh'", ?_assert(grass:edge_exists(G, <<"b">>, <<"eh">>))},
    {"'eh' knows 'd'", ?_assert(grass:edge_exists(G, <<"eh">>, <<"d">>))},
    {"'d' knows 'eh'", ?_assert(grass:edge_exists(G, <<"d">>, <<"eh">>))}
  ].

test_del_edge(G) ->
  [
    {"Delete existing edge", ?_assertEqual(ok, grass:del_edge(G, <<"eh">>, <<"d">>))},
    {"Delete non existing edge", ?_assertEqual(ok, grass:del_edge(G, <<"a">>, <<"d">>))},
    {"Deleted edge gone", ?_assertNot(grass:edge_exists(G, <<"eh">>, <<"d">>))},
    {"Non delete edge to 'a' preserved", ?_assert(grass:edge_exists(G, <<"eh">>, <<"a">>))},
    {"Non delete edge to 'b' preserved", ?_assert(grass:edge_exists(G, <<"b">>, <<"eh">>))}
  ].

test_del_vertex(G) ->
  [
    {"Delete vertex 'eh'", ?_assertEqual(ok, grass:del_vertex(G, <<"eh">>))},
    {"All edges of 'eh' gone", ?_assertEqual([[<<"a">>, <<"b">>], [<<"a">>, <<"c">>], [<<"b">>, <<"c">>], [<<"c">>, <<"d">>]], grass:edges(G))},
    {"All verticies preserved", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass:verticies(G))}
  ].

test_drop_and_is_empty(G) ->
  [
    {"Drop", ?_assertEqual(ok, grass:drop(G))},
    {"IsEmpty", ?_assert(grass:is_empty(G))},
    {"It is really empty", ?_assertEqual([], grass:verticies(G))}
  ].

-endif.