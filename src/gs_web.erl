-module(gs_web).

-export([init/1, allowed_methods/2, content_types_provided/2, resource_exists/2, to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, []}.

allowed_methods(RD, Ctx) ->
  {['GET'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
   {[{"text/plain", to_text}], RD, Ctx}.

resource_exists(RD, Ctx) ->
  case wrq:get_qs_value("graph", RD) of
    undefined -> {true, RD, {list, grass:graphs()}};
    G ->
      Graph = list_to_binary(G),
      case grass:verticies(Graph)  of
          {error, not_found} ->
            {false, RD, Ctx};
          Vs ->
            Verticies = [ {V, grass:tags(Graph, V)} || V <- Vs],
            {true, RD, {graph, Graph, Verticies, grass:edges(Graph)}}
      end
  end.

to_text(RD, {list, List}) ->
  Response = string:join([ binary_to_list(G) || G <- List ], ", "),
  {Response, RD, {list, List}};
to_text(RD, {graph, Name, V, E}) ->
  Verticies = [ io_lib:format("\t\"~s\" [~s];~n", [A, attributes(B)]) || {A, B} <- V],
  Edges = [ io_lib:format("\t\"~s\" -- \"~s\";~n", [A, B]) || [A, B] <- E],
  Opts = "\tnode [ shape = polygon, sides = 4, fontname = \"Helvetica-Outline\" ];",
  Response = io_lib:format("graph ~s {~n~s~n~s~s}~n", [Name, Opts, Verticies, Edges]),
  {Response, RD, {graph, Name, E}};
to_text(RD, Ctx) ->
  {"Can't serialize", RD, Ctx}.

 attributes(List) ->
  L = lists:map(
    fun
      ({K, V}) when is_integer(V) -> io_lib:format("~s=~b", [K, V]);
      ({K, V}) -> io_lib:format("~s=\"~s\"", [K, V])
    end,
    List),
  string:join(L, ", ").
