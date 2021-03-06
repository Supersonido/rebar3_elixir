%%%-------------------------------------------------------------------
%% @doc example public API
%% @end
%%%-------------------------------------------------------------------

-module(example_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  io:format("~p",['Elixir.Decimal':new(<<"1.32">>)]),
  example_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
