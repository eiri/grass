-module(grass_utils).

-export([find_path/3, find_short_path/3]).

-type graph() :: atom() | pid().
-type vertex() :: binary().
-type path() :: [vertex(),...] | [].
-type error() :: term() | {eleveldb, binary()}.

%% @doc Find path between verticies 'From' and 'To' in the graph G.
%%  Implementation of depth-first search algorithm. 
-spec find_path(graph(), vertex(), vertex()) -> path() | {error, error()}.
find_path(G, From, To) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> grass_utils:find_path(Pid, From, To);
    E -> E
  end;
find_path(G, From, To) ->
  %% Adjacents, EndPoing, Path, Visited, Forks 
  find_path(G, grass:verticies(G, From), To, [From], [From], []).

find_path(_, [To|_], To, Path, _, _) ->
  lists:reverse([To|Path]);
find_path(G, [Next|Rest], To, Path, Visited, Forks) ->
  case lists:member(Next, Visited) of
    true ->
      find_path(G, Rest, To, Path, Visited, Forks);
    false ->
      find_path(G, grass:verticies(G, Next), To, [Next|Path],
                             [Next|Visited], [{Rest,Path}|Forks])
  end;
find_path(G, [], To, _, Visited, [{Rest,Path}|Forks]) ->
  find_path(G, Rest, To, Path, Visited, Forks);
find_path(_, [], _, _, _, []) -> [].

%% @doc Find shortest path between verticies 'From' and 'To' in the graph G. 
%%  Implementation of breadth-first search algorithm. 
-spec find_short_path(graph(), vertex(), vertex()) -> path() | {error, error()}.
find_short_path(G, From, To) when is_binary(G) ->
  case gs_register_server:get(G) of
    {ok, Pid} -> grass_utils:find_short_path(Pid, From, To);
    E -> E
  end;
find_short_path(G, From, To) ->
  find_short_path(G, adj_to_queue(G, [From], From, queue:new()), To, []).

find_short_path(G, Q, To, Visited) ->
  case queue:out(Q) of
    {{value, {To, Path}}, _} ->
      lists:reverse(Path);
    {{value, {Next, Path}}, Q1} ->
      case lists:member(Next, Visited) of
        true ->
          find_short_path(G, Q1, To, Visited);
        false ->
          Q2 = adj_to_queue(G, Path, Next, Q1),
          find_short_path(G, Q2, To, [Next|Visited])
        end;
    {empty, _} ->
      []
  end.

adj_to_queue(G, Path, V, Q0) ->
  Fold = fun(W, Q) -> queue:in({W, [W|Path]}, Q) end,
  lists:foldl(Fold, Q0, grass:verticies(G, V)).

%%
%% EUnit tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

utils_test_() ->
  {setup,
    fun test_setup/0,
    fun test_teardown/1,
    fun(Data) ->
      Pid = proplists:get_value(ref, Data),
      {inorder, [
        test_find_path(Pid),
        test_find_short_path(Pid)
      ]}
    end
  }.

test_setup() ->
  WD = os:cmd("mktemp -d /tmp/grass.XXXXXX"),
  WorkDir = string:strip(WD, right, $\n),
%%  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])),
  ?debugFmt("Work Dir: ~p~n", [WorkDir]),
  {ok, Ref} = gs_graph_server:start_link([{<<"test">>, WorkDir}]),
  %% create graph
  %% verticies
  grass:add_vertex(Ref, <<"a">>),
  grass:add_vertex(Ref, <<"b">>),
  grass:add_vertex(Ref, <<"c">>),
  grass:add_vertex(Ref, <<"d">>),
  grass:add_vertex(Ref, <<"e">>),
  grass:add_vertex(Ref, <<"f">>),
  grass:add_vertex(Ref, <<"b1">>),
  grass:add_vertex(Ref, <<"c1">>),
  grass:add_vertex(Ref, <<"x">>),
  grass:add_vertex(Ref, <<"y">>),
  grass:add_vertex(Ref, <<"z">>),
  %% edges
  grass:add_edge(Ref, <<"a">>, <<"b">>),
  grass:add_edge(Ref, <<"a">>, <<"b1">>),
  grass:add_edge(Ref, <<"b">>, <<"c">>),
  grass:add_edge(Ref, <<"b1">>, <<"c1">>),
  grass:add_edge(Ref, <<"c">>, <<"d">>),
  grass:add_edge(Ref, <<"c1">>, <<"d">>),
  grass:add_edge(Ref, <<"c1">>, <<"e">>),
  grass:add_edge(Ref, <<"e">>, <<"f">>),
  grass:add_edge(Ref, <<"x">>, <<"y">>),
  grass:add_edge(Ref, <<"y">>, <<"z">>),
  %% check graph
  ?assertEqual([
    [<<"a">>, <<"b">>],
    [<<"a">>, <<"b1">>],
    [<<"b">>, <<"c">>],
    [<<"b1">>, <<"c1">>],
    [<<"c">>, <<"d">>],
    [<<"c1">>, <<"d">>],
    [<<"c1">>, <<"e">>],
    [<<"e">>, <<"f">>],
    [<<"x">>, <<"y">>],
    [<<"y">>, <<"z">>]], grass:edges(Ref)),
  [{dir, WorkDir}, {ref, Ref}].

test_teardown(TestData) ->
  WorkDir = proplists:get_value(dir, TestData),
  Ref = proplists:get_value(ref, TestData),
  gs_graph_server:stop(Ref),
  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])).

test_find_path(G) ->
  [
    {"Short path", ?_assertEqual([<<"a">>, <<"b">>], grass_utils:find_path(G, <<"a">>, <<"b">>))},
    {"Long path", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass_utils:find_path(G, <<"a">>, <<"d">>))},
    {"Long-long path", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"c1">>, <<"e">>, <<"f">>], grass_utils:find_path(G, <<"a">>, <<"f">>))},
    {"Cyclic path", ?_assertEqual([<<"a">>, <<"b">>, <<"a">>], grass_utils:find_path(G, <<"a">>, <<"a">>))},
    {"Empty path", ?_assertEqual([], grass_utils:find_path(G, <<"a">>, <<"z">>))}
  ].

test_find_short_path(G) ->
  [
    {"Short path", ?_assertEqual([<<"a">>, <<"b">>], grass_utils:find_short_path(G, <<"a">>, <<"b">>))},
    {"Long path", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass_utils:find_short_path(G, <<"a">>, <<"d">>))},
    {"Long-long path", ?_assertEqual([<<"a">>, <<"b1">>, <<"c1">>, <<"e">>, <<"f">>], grass_utils:find_short_path(G, <<"a">>, <<"f">>))},
    {"Cyclic path", ?_assertEqual([<<"a">>, <<"b">>, <<"a">>], grass_utils:find_short_path(G, <<"a">>, <<"a">>))},
    {"Empty path", ?_assertEqual([], grass_utils:find_short_path(G, <<"a">>, <<"z">>))}
  ].

-endif.