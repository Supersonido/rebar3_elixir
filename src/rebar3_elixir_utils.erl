-module(rebar3_elixir_utils).

-export([to_binary/1, get_env/1, compile_app/2, move_deps/1]).

-spec to_binary(binary() | list() | integer() | atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).

-spec get_env(any()) -> atom().
get_env(State) ->
  Config = rebar_state:get(State, elixir_opts, []),
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

-spec get_bin_dir(any()) -> string().
get_bin_dir(State) ->
  Config = rebar_state:get(State, elixir_opts, []),
  case lists:keyfind(bin_dir, 1, Config) of
    false -> 
      {ok, ElixirBin_} = find_executable("elixir"),
      filename:dirname(ElixirBin_);
    {bin_dir, Dir1} -> Dir1
  end.
  
-spec compile_app(any(), string()) -> {ok, atom()} | error.
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

-spec move_deps(any()) -> ok.
move_deps(State) ->
  BaseDir = filename:join([rebar_dir:root_dir(State), "_elixir_build/"]),  %% Base app.
  {ok, Dirs} = rebar_utils:list_dir(BaseDir),
  Env = get_env(State),
  BuildPath = filename:join([rebar_dir:root_dir(State), "_build/", "default/lib"]),
  lists:map(
    fun(Dir) -> 
        DirPath = filename:join([BaseDir, Dir, "_build/", Env, "lib"]),
        {ok, Deps} = rebar_utils:list_dir(DirPath),
        
        lists:map(
          fun(Dep) ->
              Source = filename:join([DirPath, Dep]),
              Target = filename:join([BuildPath, Dep]),              
              ec_file:copy(Source, Target, [recursive])
          end,
          Deps -- [Dir])
          
    end, Dirs),
  State.
%%=============================
%% Private functions
%%=============================
find_executable(Name) ->
  case os:find_executable(Name) of
    false -> false;
    Path -> {ok, filename:nativename(Path)}
  end.
