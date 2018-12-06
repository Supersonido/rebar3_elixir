-module(rebar3_elixir_git).

-behaviour(rebar_resource_v2).

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

%% Init
init(Type, _State) ->
  Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
  {ok, Resource}.


%% Lock
lock(_, _) ->
  ok.


%% Download download
download(_, _, _, _) ->
  ok.


%% Needs Update
needs_update(_, _) ->
  ok.


%%
make_vsn(_, _) ->
  ok.
