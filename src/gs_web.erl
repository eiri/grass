-module(gs_web).

-export([init/1, allowed_methods/2, content_types_provided/2, to_text/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

allowed_methods(RD, Ctx) ->
  {['GET'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
   {[{"text/plain",to_text}], RD, Ctx}.

to_text(RD, Ctx) ->
  Edges = [ io_lib:format("\t\"~s\" -- \"~s\";~n", [A, B]) || [A, B] <- grass:edges() ],
  Response = io_lib:format("graph G {~n~s}~n", [Edges]),
  {Response, RD, Ctx}.

 