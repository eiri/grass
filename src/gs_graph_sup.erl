-module(gs_graph_sup).

-behaviour(supervisor).

-define(WORKER(N, A), {N, {gs_graph_server, start_link, [A]}, transient, 2000, worker, [gs_graph_server]}).

-export([start_link/0, init/1, start_child/2, stop_child/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(GraphName, GraphDir) ->
  supervisor:start_child(?MODULE, ?WORKER(GraphName, [{GraphName, GraphDir}])).

stop_child(GraphName) ->
  supervisor:terminate_child(?MODULE, GraphName),
  supervisor:delete_child(?MODULE, GraphName).


init([]) ->
  {ok, {{one_for_one, 3, 3600}, []}}.