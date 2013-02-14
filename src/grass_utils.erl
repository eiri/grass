-module(grass_utils).

-export([find_path/2, find_short_path/2]).

-type vertex() :: binary().
-type path() :: [vertex(),...] | [].
-type error() :: term() | {eleveldb, binary()}.

%% @doc Find path between verticies 'From' and 'To'.
%%  Implementation of depth-first search algorithm. 
-spec find_path(vertex(), vertex()) -> path() | {error, error()}.
find_path(From, To) ->
  %% Adjacents, EndPoing, Path, Visited, Forks 
  find_path(grass:verticies(From), To, [From], [From], []).

find_path([To|_], To, Path, _, _) ->
  lists:reverse([To|Path]);
find_path([Next|Rest], To, Path, Visited, Forks) ->
  case lists:member(Next, Visited) of
    true ->
      find_path(Rest, To, Path, Visited, Forks);
    false ->
      find_path(grass:verticies(Next), To, [Next|Path],
                      [Next|Visited], [{Rest,Path}|Forks])
  end;
find_path([], To, _, Visited, [{Rest,Path}|Forks]) ->
  find_path(Rest, To, Path, Visited, Forks);
find_path([], _, _, _, []) -> [].

%% @doc Find shortest path between verticies 'From' and 'To'.
%%  Implementation of breadth-first search algorithm. 
-spec find_short_path(vertex(), vertex()) -> path() | {error, error()}.
find_short_path(From, To) ->
  find_short_path(adj_to_queue([From], From, queue:new()), To, []).

find_short_path(Q, To, Visited) ->
  case queue:out(Q) of
    {{value, {To, Path}}, _} ->
      lists:reverse(Path);
    {{value, {Next, Path}}, Q1} ->
      case lists:member(Next, Visited) of
        true ->
          find_short_path(Q1, To, Visited);
        false ->
          Q2 = adj_to_queue(Path, Next, Q1),
          find_short_path(Q2, To, [Next|Visited])
        end;
    {empty, _} ->
      []
  end.

adj_to_queue(Path, V, Q0) ->
  Fold = fun(W, Q) -> queue:in({W, [W|Path]}, Q) end,
  lists:foldl(Fold, Q0, grass:verticies(V)).

%%
%% EUnit tests
%%

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

utils_test_() ->
  {setup,
    fun test_setup/0,
    fun test_teardown/1,
    fun(_Pid) ->
      {inorder, [
        test_find_path(),
        test_find_short_path()
      ]}
    end
  }.

test_setup() ->
  WD = os:cmd("mktemp -d /tmp/grass.XXXXXX"),
  WorkDir = string:strip(WD, right, $\n),
%%  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])),
  ?debugFmt("Work Dir: ~p~n", [WorkDir]),
  {ok, Ref} = gs_graph_server:start_link([WorkDir]),
  %% create graph
  %% verticies
  grass:add_vertex(<<"a">>),
  grass:add_vertex(<<"b">>),
  grass:add_vertex(<<"c">>),
  grass:add_vertex(<<"d">>),
  grass:add_vertex(<<"e">>),
  grass:add_vertex(<<"f">>),
  grass:add_vertex(<<"b1">>),
  grass:add_vertex(<<"c1">>),
  grass:add_vertex(<<"x">>),
  grass:add_vertex(<<"y">>),
  grass:add_vertex(<<"z">>),
  %% edges
  grass:add_edge(<<"a">>, <<"b">>),
  grass:add_edge(<<"a">>, <<"b1">>),
  grass:add_edge(<<"b">>, <<"c">>),
  grass:add_edge(<<"b1">>, <<"c1">>),
  grass:add_edge(<<"c">>, <<"d">>),
  grass:add_edge(<<"c1">>, <<"d">>),
  grass:add_edge(<<"c1">>, <<"e">>),
  grass:add_edge(<<"e">>, <<"f">>),
  grass:add_edge(<<"x">>, <<"y">>),
  grass:add_edge(<<"y">>, <<"z">>),
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
    [<<"y">>, <<"z">>]], grass:edges()),
  [{dir, WorkDir}, {ref, Ref}].

test_teardown(TestData) ->
  WorkDir = proplists:get_value(dir, TestData),
  Ref = proplists:get_value(ref, TestData),
  gs_graph_server:stop(Ref),
  os:cmd(io_lib:format("rm -rf ~p", [WorkDir])).

test_find_path() ->
  [
    {"Short path", ?_assertEqual([<<"a">>, <<"b">>], grass_utils:find_path(<<"a">>, <<"b">>))},
    {"Long path", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass_utils:find_path(<<"a">>, <<"d">>))},
    {"Long-long path", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"c1">>, <<"e">>, <<"f">>], grass_utils:find_path(<<"a">>, <<"f">>))},
    {"Cyclic path", ?_assertEqual([<<"a">>, <<"b">>, <<"a">>], grass_utils:find_path(<<"a">>, <<"a">>))},
    {"Empty path", ?_assertEqual([], grass_utils:find_path(<<"a">>, <<"z">>))}
  ].

test_find_short_path() ->
  [
    {"Short path", ?_assertEqual([<<"a">>, <<"b">>], grass_utils:find_short_path(<<"a">>, <<"b">>))},
    {"Long path", ?_assertEqual([<<"a">>, <<"b">>, <<"c">>, <<"d">>], grass_utils:find_short_path(<<"a">>, <<"d">>))},
    {"Long-long path", ?_assertEqual([<<"a">>, <<"b1">>, <<"c1">>, <<"e">>, <<"f">>], grass_utils:find_short_path(<<"a">>, <<"f">>))},
    {"Cyclic path", ?_assertEqual([<<"a">>, <<"b">>, <<"a">>], grass_utils:find_short_path(<<"a">>, <<"a">>))},
    {"Empty path", ?_assertEqual([], grass_utils:find_short_path(<<"a">>, <<"z">>))}
  ].

-endif.