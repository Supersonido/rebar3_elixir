-module(rebar3_elixir_dep).

-behaviour(rebar_resource_v2).

-define(DEFAULT_CDN_SITE, "https://repo.hex.pm").
-define(CDN_TARBALL_LOCATION, "/tarballs").

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
lock(AppInfo, _) ->
  {iex_dep, Name, Vsn} = rebar_app_info:source(AppInfo),
  {iex_dep, rebar3_elixir_utils:to_binary(Name), rebar3_elixir_utils:to_binary(Vsn)}.


%% Download download
download(TmpDir, AppInfo, ResorceState, _State) ->
  ok.
  

%% Needs Update
needs_update(AppInfo, _) ->
  false.


%% Make VSN
make_vsn(_, _) ->
  {error, "Replacing version of type elixir not supported."}.


%%=================================
%% Private function
%%=================================
