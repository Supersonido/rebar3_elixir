-module(rebar3_elixir_compiler).

-export([build/1,
         format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================
build(AppInfo) ->
  AppDir = rebar_app_info:dir(AppInfo),
  BuildDir = filename:join(AppDir, "../"),
  rebar_utils:sh("mix deps.get", [
                                  {cd, AppDir}, 
                                  {use_stdout, false}, 
                                  abort_on_error]),
  rebar_utils:sh("mix compile", [
                                 {cd, AppDir}, 
                                 {use_stdout, false}, 
                                 abort_on_error,
                                 {env, [
                                        {"MIX_ENV", rebar3_elixir_utils:get_env()}
                                       ]
                                 }]),
  ok.

format_error({mix_not_found, Name}) ->
  io_lib:format("Elixir and mix must be installed to build application ~ts. "
                "Install Elixir or check your path and try again.", [Name]);
format_error({mix_compile_failed, Name, _Error}) ->
  io_lib:format("Failed to compile application ~ts with mix", [Name]);
format_error(Reason) ->
  io_lib:format("~p", Reason).
