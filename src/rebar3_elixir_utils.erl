-module(rebar3_elixir_utils).

-export([to_binary/1, 
         get_env/1, 
         compile_app/2, 
         move_deps/1, 
         move_deps/3, 
         add_elixir/1, 
         %%add_deps_to_lock/1, 
         create_rebar_lock_from_mix/1, 
         create_rebar_lock_from_mix/2]).

-spec to_binary(binary() | list() | integer() | atom()) -> binary().
to_binary(V) when is_binary(V) -> V;
to_binary(V) when is_list(V) -> list_to_binary(V);
to_binary(V) when is_integer(V) -> integer_to_binary(V);
to_binary(V) when is_atom(V) -> atom_to_binary(V, latin1);
to_binary(_) -> erlang:error(badarg).

-spec get_env(any()) -> atom().
get_env(_State) ->
  case os:getenv("MIX_ENV") of
    false ->
      prod;
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

-spec get_lib_dir(any()) -> string().
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
  Code =
    case rebar_utils:list_dir(BaseDir) of
      {ok, Dirs} ->
        Env = get_env(State),
        BuildPath = filename:join([rebar_dir:root_dir(State), "_build/", "default/lib"]),
        lists:foldl(
          fun(Dir, Acc) -> 
              DirPath = filename:join([BaseDir, Dir, "_build/", Env, "lib"]),
              {ok, Deps} = rebar_utils:list_dir(DirPath),
            
              lists:foldl(
                fun(Dep, Acc1) ->
                    Source = filename:join([DirPath, Dep]),
                    Target = filename:join([BuildPath, Dep]),              
                    ec_file:copy(Source, Target, [recursive]),
                    Acc1 ++ [filename:join([Target, "ebin"])]
                end, Acc, Deps -- [Dir])
                
          end, [], Dirs);
      _ ->
        []
    end,
  EbinList = rebar_state:code_paths(State, all_deps),
  EbinFull = EbinList ++ Code,
  rebar_state:code_paths(State, all_deps, EbinFull).

move_deps(Deps, Dir, State) ->
  BuildPath = filename:join([rebar_dir:root_dir(State), "_build/", "default/lib"]),
  lists:map(
    fun(Dep) ->
        Source = filename:join([Dir, Dep]),
        Target = filename:join([BuildPath, Dep]),              
        ec_file:copy(Source, Target, [recursive])
    end, Deps).

%% -spec add_deps_to_lock(any()) -> any().
%% add_deps_to_lock(State) ->
%%   BaseDir = filename:join([rebar_dir:root_dir(State), "_elixir_build/"]),  %% Base app.
%%   Apps = 
%%     case rebar_utils:list_dir(BaseDir) of
%%       {ok, Dirs} ->
%%         Env = get_env(State),
%%         BuildPath = filename:join([rebar_dir:root_dir(State), "_build/", "default/lib"]),
        
%%         %% Do for all _elixir_build/ deps
%%         lists:foldl(
%%           fun(Parent, Acc) -> 
%%               DirPath = filename:join([BaseDir, Parent, "_build/", Env, "lib"]),
%%               io:format("~p~n~n~n", [get_mix_lock(filename:join([BaseDir, Parent]))]),
%%               {ok, Deps} = rebar_utils:list_dir(DirPath),  %% All compiled files for Parent
%%               {ok, ParentApp} = get_app(to_binary(Parent), Acc),
              
%%               %% For all parents deps
%%               {NewState, NewParent} = 
%%                 lists:foldl(
%%                   fun(Dep, {Acc1, Accparent1}) ->
%%                       Target = filename:join([BuildPath, Dep]),  %% Build Path
                      
%%                       %% Create application
%%                       {ok, AppInfo} = rebar_app_info:new(to_binary(Dep), <<"aa">>, Target),
%%                       AppInfo2 = rebar_app_info:dep_level(AppInfo, 1),
%%                       AppInfo3 = rebar_app_info:source(AppInfo2, {iex, to_binary(Dep), <<"aa">>}),
%%                       AppInfo4 = rebar_app_info:parent(AppInfo3, to_binary(Parent)),
%%                       AppInfo5 = rebar_app_info:is_checkout(AppInfo4, false),
                      
%%                       %% Find application on build path
%%                       case rebar_app_discover:find_app(AppInfo5, Target, all) of
%%                         {true, AppInfo_} -> 
%%                           case is_member(to_binary(Dep), Acc1) of
%%                             false -> 
%%                               %%io:format("~p~n", [rebar_app_info:is_checkout(AppInfo_)]),
%%                               %%AppInfo_ = rebar_app_info:is_checkout(AppInfo_0, true),
%%                               %% Add dep into Parent
%%                               ParentLock0 = rebar_app_info:get(Accparent1, {deps,default}),
%%                               ParentLock1 = rebar_app_info:get(Accparent1, {locks,default}, []),
%%                               ParentLockUp0 = ParentLock0 ++ [{to_binary(Dep) ,{iex, to_binary(Dep), <<"aa">>}, 0}],
%%                               ParentLockUp1 = ParentLock1 ++ [{to_binary(Dep) ,{iex, to_binary(Dep), <<"aa">>}, 0}],
                              
%%                               %% Add dep into lock
%%                               OldLocks = rebar_state:get(State, {locks, default}, []),
%%                               NewLocks = OldLocks ++ [{to_binary(Dep) ,{iex, to_binary(Dep), <<"aa">>}, 1}],
%%                               Acc2 = rebar_state:set(State, {locks, default}, NewLocks),
                              
%%                               %%
%%                               PP1 = rebar_app_info:set(Accparent1, {deps,default}, ParentLockUp0),
%%                               {rebar_state:lock(Acc1, AppInfo_), rebar_app_info:set(PP1, {locks,default}, ParentLockUp1)};
%%                             true -> 
%%                               {Acc1, Accparent1}
%%                           end;
%%                         _ ->  
%%                         {Acc1, Accparent1}
%%                       end
%%                   end, {Acc, ParentApp}, Deps -- [Parent]),
              
%%               %% Update App into state
%%               AllDeps = delete_app(to_binary(Parent), rebar_state:all_deps(NewState)),
%%               %%io:format("~p~n", [AllDeps]),
%%               State10 = rebar_state:all_deps(NewState, AllDeps ++ [NewParent]),
%%               rebar_state:lock(State10, NewParent)
%%           end, State, Dirs);
%%       _ ->
%%         State
%%     end,
%%   Apps.

-spec create_rebar_lock_from_mix(string()) -> ok | {error, term()}.
create_rebar_lock_from_mix(AppDir) ->
  create_rebar_lock_from_mix(AppDir, AppDir).

-spec create_rebar_lock_from_mix(string(), string()) -> ok | {error, term()}.
create_rebar_lock_from_mix(AppDir, TargetDir) ->
  MixLocks = get_mix_lock(AppDir),
  io:format("~p~n", [MixLocks]),
  RebarLocks = 
    lists:foldl(
      fun(AppLock, Locks) ->
          case AppLock of
            {Name, {hex, App, Version, _, _, _, _}} ->
              Locks ++ [{Name, {iex_dep, App, Version}, 0}];
            {Name, {git, App, URL, Hash, _}} ->
              Locks ++ [{Name, {iex_dep, URL, Hash}, 0}];
            _->
              Locks
          end
      end, [], MixLocks),
  rebar_config:write_lock_file(filename:join(TargetDir, "rebar.lock"), RebarLocks).
  

-spec add_elixir(any()) -> any().
add_elixir(State) ->
  LibDir = get_lib_dir(State),
  code:add_patha(filename:join(LibDir, "elixir/ebin")),
  code:add_patha(filename:join(LibDir, "mix/ebin")),
  code:add_patha(filename:join(LibDir, "logger/ebin")),
  State.

%%=============================
%% Private functions
%%=============================

%% Return the filepath of an executable file
find_executable(Name) ->
  case os:find_executable(Name) of
    false -> false;
    Path -> {ok, filename:nativename(Path)}
  end.

%% Check if exist in lock
is_member(Name, State) ->
  is_member(Name, State, rebar_state:lock(State)).

is_member(_Name, _State, []) ->
  false;

is_member(Name, State, [App | Apps]) ->
  case Name == rebar_app_info:name(App) of
    true -> true;
    false -> is_member(Name, State, Apps)
  end.


%% Return the app_info_t from state
get_app(Name, State) ->
  get_app(Name, State, rebar_state:all_deps(State)).

get_app(Name, State, [App | Apps]) ->
  case rebar_app_info:name(App) of
    Name ->
      {ok, App};
    _ ->
      get_app(Name, State, Apps)
  end;

get_app(Name, State, []) ->
  false.

%% Delete the app_info_t from state.
delete_app(Name, AppList) ->
  delete_app(Name, AppList, []).

delete_app(Name, [App | Apps], New) ->
  case rebar_app_info:name(App) of
    Name ->
      New;
    _ ->
     delete_app(Name, Apps, New ++ [App])
  end;

delete_app(Name, [], New) ->
  New.

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
