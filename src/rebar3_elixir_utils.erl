-module(rebar3_elixir_utils).

-export([to_binary/1, 
         to_string/1,
         get_env/1,
         compile_app/2,
         move_deps/3,
         add_elixir/1,
         get_build_path/1,
         create_rebar_lock_from_mix/2,
         save_rebar_lock/2,
         add_elixir_to_dependence/2,
         %% NEW
         get_deps/1,
         compile/1,
         move_to_path/3
        ]).

-spec to_binary(binary() | list() | integer() | atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).

-spec to_string(binary() | list() | integer() | atom()) -> string().
to_string(Value) when is_binary(Value) -> binary_to_list(Value);
to_string(Value) when is_list(Value) -> Value;
to_string(Value) when is_integer(Value) -> lists:flatten(io_lib:format("~p", [Value]));
to_string(Value) when is_atom(Value) -> atom_to_list(Value);
to_string(_) -> erlang:error(badarg).

-spec get_env(rebar_state:t()) -> rebar_state:t().
get_env(State) ->
  Config = rebar_state:get(State, iex_opts, []),
  case os:getenv("MIX_ENV") of
    false ->
      case lists:keyfind(env, 1, Config) of
        {env, E} ->
          E;
        _ ->
          prod
      end;
    E ->
      list_to_atom(E)
  end.

-spec profile(atom()) -> string().
profile(Env) ->
  case Env of
    dev -> ""; 
    prod -> "env MIX_ENV=" ++ atom_to_list(Env)
  end.   

-spec get_bin_dir(rebar_state:t()) -> string().
get_bin_dir(State) ->
  Config = rebar_state:get(State, iex_opts, []),
  case lists:keyfind(bin_dir, 1, Config) of
    false -> 
      {ok, ElixirBin_} = find_executable("elixir"),
      filename:dirname(ElixirBin_);
    {bin_dir, Dir1} -> Dir1
  end.

-spec get_lib_dir(rebar_state:t()) -> string().
get_lib_dir(State) ->
  Config = rebar_state:get(State, elixir_opts, []),
  case lists:keyfind(lib_dir, 1, Config) of
    false -> 
      case rebar_utils:sh("elixir -e \"IO.puts :code.lib_dir(:elixir)\"", [return_on_error]) of
        {ok, ElixirLibs_} ->
          filename:join(re:replace(ElixirLibs_, "\\s+", "", [global,{return,list}]), "../");
        _ ->
          "/usr/local/lib/elixir/bin/../lib/elixir"
      end;
    {lib_dir, Dir2} -> Dir2
  end.

-spec get_build_path(rebar_state:t()) -> string().
get_build_path(State) ->
  [Profile | _] = rebar_state:current_profiles(State),
  filename:join([rebar_dir:root_dir(State), "_build/", to_string(Profile), "lib"]).

-spec compile_app(rebar_state:t(), string()) -> {ok, atom()} | error.
compile_app(State, Dir) ->
  Env = get_env(State),
  Profile = profile(Env),
  BinDir = get_bin_dir(State),
  Mix = filename:join(BinDir, "mix"),
  case ec_file:exists(filename:join(Dir, "mix.exs")) of
    true ->
      rebar_utils:sh(Profile ++ " " ++ Mix ++ " deps.get", [{cd, Dir}, {use_stdout, false}, abort_on_error]),
      rebar_utils:sh(Profile ++ " " ++ Mix ++ " compile", [{cd, Dir}, {use_stdout, false}, abort_on_error]),
      {ok, Env};
    false ->
      error
  end.

-spec move_deps(list(), string(), rebar_state:t()) -> list().
move_deps(Deps, Dir, State) ->
  BuildPath = get_build_path(State),
  lists:map(
    fun(Dep) ->
        Source = filename:join([Dir, Dep]),
        Target = filename:join([BuildPath, Dep]),              
        ec_file:copy(Source, Target, [recursive])
    end, Deps).

-spec create_rebar_lock_from_mix(string(), list()) -> ok | {error, term()}.
create_rebar_lock_from_mix(AppDir, Deps) ->
  MixLocks = get_mix_lock(AppDir),
  lists:foldl(
    fun(AppLock, Locks) ->
        case AppLock of
          {Name, {hex, App, Version, _, _, _, _}} ->
            case lists:member(to_string(Name), Deps) of
              true ->
                Locks ++ [{Name, {iex_dep, App, Version}, 0}];
              false ->
                Locks
            end;
          {Name, {git, URL, Hash, _}} ->
            case lists:member(to_string(Name), Deps) of
              true ->
                Locks ++ [{Name, {iex_dep, URL, Hash}, 0}];
              false ->
                Locks
            end;
          _->
            Locks
        end
    end, [], MixLocks).

-spec save_rebar_lock(string(), list()) -> ok | {error, term()}.
save_rebar_lock(Dir, Locks) ->
  rebar_config:write_lock_file(filename:join(Dir, "rebar.lock"), Locks).

-spec add_elixir(rebar_state:t()) -> rebar_state:t().
add_elixir(State) ->
  LibDir = get_lib_dir(State),
  code:add_patha(filename:join(LibDir, "elixir/ebin")),
  code:add_patha(filename:join(LibDir, "mix/ebin")),
  code:add_patha(filename:join(LibDir, "logger/ebin")),
  State.

-spec add_elixir_to_dependence(rebar_state:t(), list()) -> list().
add_elixir_to_dependence(State, Locks)->
  LibDir = get_lib_dir(State),
  BuildPath = get_build_path(State),
  
  %% Link Elixir
  ElixirPath = filename:join(LibDir, "elixir"),
  ElixirBuildPath = filename:join(BuildPath, "elixir"),
  file:make_symlink(ElixirPath, ElixirBuildPath),
  
  %% Link Logger
  LoggerPath = filename:join(LibDir, "logger"),
  LoggerBuildPath = filename:join(BuildPath, "logger"),
  file:make_symlink(LoggerPath, LoggerBuildPath),
  
  %% Link mix
  MixPath = filename:join(LibDir, "mix"),
  MixBuildPath = filename:join(BuildPath, "mix"),
  file:make_symlink(MixPath, MixBuildPath),
  
  %% Add Locks
  elixit_to_lock(Locks).

-spec elixit_to_lock(list()) -> list().
elixit_to_lock(Lock) ->   
  Lock ++ 
    [
     {elixir, {iex_dep, <<"elixir">>, <<"">>}, 0},
     {logger, {iex_dep, <<"logger">>, <<"">>}, 0},
     {mix, {iex_dep, <<"mix">>, <<"">>}, 0}
    ].

%% NEW CODE
-spec get_deps(string()) -> [string()].
get_deps(Path) ->
  DepsDir = filename:join(Path, "deps"),
  {ok, Deps} = rebar_utils:list_dir(DepsDir),
  Deps.


-spec compile(string()) -> ok.
compile(AppDir) ->
  {ok, _ } = rebar_utils:sh("mix deps.get", 
                            [
                             {cd, AppDir}, 
                             {use_stdout, false}, 
                             abort_on_error]),
  {ok, _ } = rebar_utils:sh("mix compile", 
                            [
                             {cd, AppDir}, 
                             {use_stdout, false}, 
                             abort_on_error,
                             {env, [
                                    {"MIX_ENV", "prod"}
                                   ]
                             }]),
  ok.

move_to_path(Files, Source, Traget) ->
  lists:map(
    fun(File) ->
        Source1 = filename:join([Source, File]),
        Target1 = filename:join([Traget, File]),              
        ec_file:copy(Source1, Target1, [recursive])
    end, Files).
%%=============================
%% Private functions
%%=============================

%% Return the filepath of an executable file
find_executable(Name) ->
  case os:find_executable(Name) of
    false -> false;
    Path -> {ok, filename:nativename(Path)}
  end.

%% Get mix.lock from deps 
get_mix_lock(AppDir) ->
  Lockfile = filename:join(AppDir, "mix.lock"),
  application:ensure_all_started(elixir),
  case 'Elixir.File':read(Lockfile) of
    {ok,Info} ->
      Opts = [{file, to_binary(Lockfile)}, {warn_on_unnecessary_quotes, false}],
      {ok, Quoted} = 'Elixir.Code':string_to_quoted(Info, Opts),
      {EvalRes, _Binding} = 'Elixir.Code':eval_quoted(Quoted, Opts),
      'Elixir.Enum':to_list(EvalRes);
    {error, _} ->
      []
  end.
