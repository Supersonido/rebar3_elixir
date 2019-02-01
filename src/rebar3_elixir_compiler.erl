-module(rebar3_elixir_compiler).

-export([build/1,
         format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
build(AppInfo) ->
  AppDir = rebar_app_info:dir(AppInfo),
  BuildDir = filename:join(AppDir, "../"),
  BuildElixirDir = filename:join(AppDir, "_build/prod/lib/"),
  AppName = rebar3_elixir_utils:to_string(rebar_app_info:name(AppInfo)),

  ok = rebar3_elixir_utils:compile(AppDir),

  Deps = rebar3_elixir_utils:get_deps(AppDir),
  rebar3_elixir_utils:move_to_path(Deps, BuildElixirDir, BuildDir),
  
  AppBuild = filename:join(AppDir, "_build/prod/lib/" ++ AppName ++ "/ebin"),
  AppTaget = filename:join(AppDir, "ebin"),
  ec_file:copy(AppBuild, AppTaget, [recursive]),
  
  Lock = rebar3_elixir_utils:create_rebar_lock_from_mix(AppDir, Deps),
  rebar3_elixir_utils:save_rebar_lock(AppDir, Lock),
  
  ok.

format_error({mix_not_found, Name}) ->
  io_lib:format("Elixir and mix must be installed to build application ~ts. "
                "Install Elixir or check your path and try again.", [Name]);
format_error({mix_compile_failed, Name, _Error}) ->
  io_lib:format("Failed to compile application ~ts with mix", [Name]);
format_error(Reason) ->
  io_lib:format("~p", Reason).
