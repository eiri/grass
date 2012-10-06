-module(gs_web).

-export([init/1, to_html/2]).

-include_lib("webmachine/include/webmachine.hrl").

init([]) ->
  {ok, undefined}.

to_html(RD, Ctx) ->
  {"Placeholder", RD, Ctx}.
 
 