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
      case grass:edges(list_to_binary(G)) of
          {error, not_found} -> {false, RD, Ctx};
          Edges -> {true, RD, {graph, G, Edges}}
      end
  end.

to_text(RD, {list, List}) ->
  Response = string:join([ binary_to_list(G) || G <- List ], ", "),
  {Response, RD, {list, List}};
to_text(RD, {graph, Name, E}) ->
  Edges = [ io_lib:format("\t\"~s\" -- \"~s\";~n", [A, B]) || [A, B] <- E],
  Response = io_lib:format("graph ~s {~n~s}~n", [Name, Edges]),
  {Response, RD, {graph, Name, E}};
to_text(RD, Ctx) ->
  {"Can't serialize", RD, Ctx}.

 