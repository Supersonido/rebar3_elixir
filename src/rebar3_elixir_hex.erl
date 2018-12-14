-module(rebar3_elixir_hex).

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
  {iex, Name, Vsn} = rebar_app_info:source(AppInfo),
  {iex, rebar3_elixir_utils:to_binary(Name), rebar3_elixir_utils:to_binary(Vsn)}.


%% Download download
download(TmpDir, AppInfo, ResorceState, _State) ->
  fetch_and_compile(ResorceState, TmpDir, rebar_app_info:source(AppInfo)).
  

%% Needs Update
needs_update(AppInfo, _) ->
  {iex, Name, Vsn} = rebar_app_info:source(AppInfo),
  rebar_api:console("Checking for update, ~p", Name),
  rebar_api:console("~p : ~p~n", [rebar_app_info:original_vsn(AppInfo), ec_cnv:to_list(Vsn)]),
  case rebar_app_info:original_vsn(AppInfo) =:= ec_cnv:to_list(Vsn) of
    true ->
      false;
    false ->
      true
  end.


%% Make VSN
make_vsn(_, _) ->
  {error, "Replacing version of type elixir not supported."}.


%%=================================
%% Private function
%%=================================
fetch_and_compile(State, Dir, {iex, Name, _Vsn} = Pkg) ->
  CDN = cdn(State),  %% Get CDN
  BaseDir = filename:join([rebar_dir:root_dir(State), "_elixir_build/", Name]),  %% Base app.
  fetch(Pkg, CDN, BaseDir), %% Fech from hex
  case rebar3_elixir_utils:compile_app(State, BaseDir) of  %% Compile elixir app.
    {ok, Env} ->
      %% Copy app code into rebar tmp folder
      Source = filename:join([BaseDir, "_build/", Env, "lib", Name]),
      ec_file:copy(Source, Dir, [recursive]),
      %% Copy deps into _build path
      DepsSource = filename:join([BaseDir, "_build/", Env, "lib"]),
      {ok, Files} = rebar_utils:list_dir(DepsSource),
      Deps = Files -- [Name],
      rebar3_elixir_utils:move_deps(Deps, DepsSource, State),
      %% Generate rebar.lock for elixir app
      Lock = rebar3_elixir_utils:create_rebar_lock_from_mix(BaseDir, Deps), 
      %% Add elixir as depencende
      Lock2 = rebar3_elixir_utils:add_elixir_to_dependence(State, Lock),
      %% Save Lock.
      rebar3_elixir_utils:save_rebar_lock(Dir, Lock2),
      ok;
    _ ->
      {error, <<"Something happen">>}
  end.

cdn(State) ->
  Opts = rebar_state:get(State, elixir_opts, []),
  CDNSite = proplists:get_value(cdn, Opts, ?DEFAULT_CDN_SITE),
  CDNSite ++ ?CDN_TARBALL_LOCATION.


fetch({iex, Name_, Vsn_}, CDN, Dir) ->
  %%Dir = filename:join([filename:absname("_elixir_build"), Name_]),
  Name = rebar3_elixir_utils:to_binary(Name_), 
  Vsn  = rebar3_elixir_utils:to_binary(Vsn_),
  case filelib:is_dir(Dir) of
    false ->
      Package = binary_to_list(<<Name/binary, "-", Vsn/binary, ".tar">>),
      Url = string:join([CDN, Package], "/"),
      case request(Url) of
        {ok, Binary} ->
          {ok, Contents} = extract(Binary),
          ok = erl_tar:extract({binary, Contents}, [{cwd, Dir}, compressed]);
        _ ->
          rebar_api:console("Error: Unable to fetch package ~p ~p~n", [Name, Vsn])
      end;
    true ->
      ok
  end.


extract(Binary) ->
  {ok, Files} = erl_tar:extract({binary, Binary}, [memory]),
  {"contents.tar.gz", Contents} = lists:keyfind("contents.tar.gz", 1, Files),
  {ok, Contents}.


request(Url) ->
  case httpc:request(get, {Url, []},
                     [{relaxed, true}],
                     [{body_format, binary}],
                     rebar) of
    {ok, {{_Version, 200, _Reason}, _Headers, Body}} ->
      {ok, Body};
    Error ->
      Error
  end.
