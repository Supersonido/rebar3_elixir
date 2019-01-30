-module(rebar3_elixir_git).

-behaviour(rebar_resource_v2).

-export([init/2,
         lock/2,
         download/4,
         needs_update/2,
         make_vsn/2]).

%% Rebar3 Macros
-define(SCP_PATTERN, "\\A(?<username>[^@]+)@(?<host>[^:]+):(?<path>.+)\\z").
-define(FMT(Str, Args), lists:flatten(io_lib:format(Str, Args))).
-define(ABORT(Str, Args), rebar_utils:abort(Str, Args)).
-define(DEBUG(Str, Args), rebar_log:log(debug, Str, Args)).
-define(WARN(Str, Args), rebar_log:log(warn, Str, Args)).


%% Init
init(Type, _State) ->
  Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
  {ok, Resource}.


%% Lock
lock(AppInfo, _State) ->
  check_type_support(),
  Name = rebar_app_info:name(AppInfo),
  {ok, CurrDir} = file:get_cwd(),
  BaseDir = filename:join([CurrDir, "_elixir_build/", Name]),  %% Base app.
  lock_(BaseDir, rebar_app_info:source(AppInfo)).

lock_(AppDir, {iex_git, Url, _}) ->
  lock_(AppDir, {iex_git, Url});

lock_(AppDir, {iex_git, Url}) ->
  AbortMsg = lists:flatten(io_lib:format("Locking of git dependency failed in ~ts", [AppDir])),
  Dir = rebar_utils:escape_double_quotes(AppDir),
  {ok, VsnString} =
    case os:type() of
      {win32, _} ->
        rebar_utils:sh("git --git-dir=\"" ++ Dir ++ "/.git\" --work-tree=\"" ++ Dir ++ "\" rev-parse --verify HEAD",
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}]);
      _ ->
        rebar_utils:sh("git --git-dir=\"" ++ Dir ++ "/.git\" rev-parse --verify HEAD",
                       [{use_stdout, false}, {debug_abort_on_error, AbortMsg}])
    end,
  Ref = rebar_string:trim(VsnString, both, "\n"),
  {iex_git, Url, {ref, Ref}}.


%% Download
download(TmpDir, AppInfo, State, _) ->
  check_type_support(),
  Name = rebar_app_info:name(AppInfo),
  BaseDir = filename:join([rebar_dir:root_dir(State), "_elixir_build/", Name]),  %% Base app.
  ec_file:remove(BaseDir, [recursive]), %% Remove if exists
  case download_(BaseDir, rebar_app_info:source(AppInfo), State) of
    {ok, _} ->
      case rebar3_elixir_utils:compile_app(State, BaseDir) of  %% Compile elixir app.
        {ok, Env} ->
          %% Copy app code into rebar tmp folder
          Source = filename:join([BaseDir, "_build/", Env, "lib", Name]),
          ec_file:copy(Source, TmpDir, [recursive]),
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
          rebar3_elixir_utils:save_rebar_lock(TmpDir, Lock2),
          ok;
        _ ->
          {error, <<"Something happen">>}
      end;
    {error, Reason} ->
      {error, Reason};
    Error ->
      {error, Error}
  end.

download_(Dir, {iex_git, Url}, State) ->
  download_(Dir, {iex_git, Url, {branch, "master"}}, State);
download_(Dir, {iex_git, Url, ""}, State) ->
  download_(Dir, {iex_git, Url, {branch, "master"}}, State);
download_(Dir, {iex_git, Url, {branch, Branch}}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(branch, git_vsn(), Url, Dir, Branch);
download_(Dir, {iex_git, Url, {tag, Tag}}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(tag, git_vsn(), Url, Dir, Tag);
download_(Dir, {iex_git, Url, {ref, Ref}}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(ref, git_vsn(), Url, Dir, Ref);
download_(Dir, {iex_git, Url, Rev}, _State) ->
  ok = filelib:ensure_dir(Dir),
  maybe_warn_local_url(Url),
  git_clone(rev, git_vsn(), Url, Dir, Rev).


%% Needs Update
needs_update(AppInfo, _) ->
  check_type_support(),
  Name = rebar_app_info:name(AppInfo),
  {ok, CurrDir} = file:get_cwd(),
  BaseDir = filename:join([CurrDir, "_elixir_build/", Name]),  %% Base app.
  needs_update_(BaseDir, rebar_app_info:source(AppInfo)).

needs_update_(Dir, {iex_git, Url, {tag, Tag}}) ->
  {ok, Current} = rebar_utils:sh(?FMT("git describe --tags --exact-match", []), [{cd, Dir}]),
  Current1 = rebar_string:trim(rebar_string:trim(Current, both, "\n"), both, "\r"),
  ?DEBUG("Comparing git tag ~ts with ~ts", [Tag, Current1]),
  not ((Current1 =:= Tag) andalso compare_url(Dir, Url));

needs_update_(Dir, {iex_git, Url, {branch, Branch}}) ->
  SafeBranch = rebar_utils:escape_chars(Branch),
  {ok, _} = rebar_utils:sh(?FMT("git fetch origin ~ts", [SafeBranch]), [{cd, Dir}]),
  {ok, Current} = rebar_utils:sh(?FMT("git log HEAD..origin/~ts --oneline", [SafeBranch]), [{cd, Dir}]),
  ?DEBUG("Checking git branch ~ts for updates", [Branch]),
  not ((Current =:= []) andalso compare_url(Dir, Url));

needs_update_(Dir, {iex_git, Url, "master"}) ->
  needs_update_(Dir, {git, Url, {branch, "master"}});

needs_update_(Dir, {iex_git, _, Ref}) ->
  {ok, Current} = rebar_utils:sh(?FMT("git rev-parse --short=7 -q HEAD", []), [{cd, Dir}]),
  Current1 = rebar_string:trim(rebar_string:trim(Current, both, "\n"),  both, "\r"),
  
  Ref2 = 
    case Ref of
      {ref, Ref1} ->
        Length = length(Current1),
        case Length >= 7 of
          true -> lists:sublist(Ref1, Length);
          false -> Ref1
        end;
      _ ->
        Ref
    end,
  
  ?DEBUG("Comparing git ref ~ts with ~ts", [Ref2, Current1]),
  (Current1 =/= Ref2).


%%
make_vsn(_, _) ->
  ok.


%%=============================
%% Private Functions
%%=============================
check_type_support() ->
  case get({is_supported, ?MODULE}) of
    true ->
      ok;
    _ ->
      case rebar_utils:sh("git --version", [{return_on_error, true},
                                            {use_stdout, false}]) of
        {error, _} ->
          ?ABORT("git not installed", []);
        _ ->
          put({is_supported, ?MODULE}, true),
          ok
      end
  end.


compare_url(Dir, Url) ->
  {ok, CurrentUrl} = rebar_utils:sh(?FMT("git config --get remote.origin.url", []), [{cd, Dir}]),
  CurrentUrl1 = rebar_string:trim(rebar_string:trim(CurrentUrl, both, "\n"), both, "\r"),
  {ok, ParsedUrl} = parse_git_url(Url),
  {ok, ParsedCurrentUrl} = parse_git_url(CurrentUrl1),
  ?DEBUG("Comparing git url ~p with ~p", [ParsedUrl, ParsedCurrentUrl]),
  ParsedCurrentUrl =:= ParsedUrl.


parse_git_url(Url) ->
  %% Checks for standard scp style git remote
  case re:run(Url, ?SCP_PATTERN, [{capture, [host, path], list}, unicode]) of
    {match, [Host, Path]} ->
      {ok, {Host, filename:rootname(Path, ".git")}};
    nomatch ->
      parse_git_url(not_scp, Url)
  end.

parse_git_url(not_scp, Url) ->
  UriOpts = [{scheme_defaults, [{git, 9418} | http_uri:scheme_defaults()]}],
  case http_uri:parse(Url, UriOpts) of
    {ok, {_Scheme, _User, Host, _Port, Path, _Query}} ->
      {ok, {Host, filename:rootname(Path, ".git")}};
    {error, Reason} ->
      {error, Reason}
  end.


maybe_warn_local_url(Url) ->
  WarnStr = "Local git resources (~ts) are unsupported and may have odd behaviour. "
    "Use remote git resources, or a plugin for local dependencies.",
  case parse_git_url(Url) of
    {error, no_scheme} -> ?WARN(WarnStr, [Url]);
    {error, {no_default_port, _, _}} -> ?WARN(WarnStr, [Url]);
    {error, {malformed_url, _, _}} -> ?WARN(WarnStr, [Url]);
    _ -> ok
  end.

%% Use different git clone commands depending on git --version
git_clone(branch,Vsn,Url,Dir,Branch) when Vsn >= {1,7,10}; Vsn =:= undefined ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts --single-branch",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Branch)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(branch,_Vsn,Url,Dir,Branch) ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Branch)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(tag,Vsn,Url,Dir,Tag) when Vsn >= {1,7,10}; Vsn =:= undefined ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts --single-branch",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Tag)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(tag,_Vsn,Url,Dir,Tag) ->
  rebar_utils:sh(?FMT("git clone ~ts ~ts ~ts -b ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir)),
                       rebar_utils:escape_chars(Tag)]),
                 [{cd, filename:dirname(Dir)}]);
git_clone(ref,_Vsn,Url,Dir,Ref) ->
  rebar_utils:sh(?FMT("git clone ~ts -n ~ts ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir))]),
                 [{cd, filename:dirname(Dir)}]),
  rebar_utils:sh(?FMT("git checkout -q ~ts", [Ref]), [{cd, Dir}]);
git_clone(rev,_Vsn,Url,Dir,Rev) ->
  rebar_utils:sh(?FMT("git clone ~ts -n ~ts ~ts",
                      [git_clone_options(),
                       rebar_utils:escape_chars(Url),
                       rebar_utils:escape_chars(filename:basename(Dir))]),
                 [{cd, filename:dirname(Dir)}]),
  rebar_utils:sh(?FMT("git checkout -q ~ts", [rebar_utils:escape_chars(Rev)]),
                 [{cd, Dir}]).

git_vsn() ->
  case application:get_env(rebar, git_vsn) of
    {ok, Vsn} -> Vsn;
    undefined ->
      Vsn = git_vsn_fetch(),
      application:set_env(rebar, git_vsn, Vsn),
      Vsn
  end.

git_vsn_fetch() ->
  case rebar_utils:sh("git --version",[]) of
    {ok, VsnStr} ->
      case re:run(VsnStr, "git version\\h+(\\d)\\.(\\d)\\.(\\d).*", [{capture,[1,2,3],list}, unicode]) of
        {match,[Maj,Min,Patch]} ->
          {list_to_integer(Maj),
           list_to_integer(Min),
           list_to_integer(Patch)};
        nomatch ->
          undefined
      end;
    {error, _} ->
      undefined
  end.


%% Internal functions
git_clone_options() ->
  Option = case os:getenv("REBAR_GIT_CLONE_OPTIONS") of 
             false -> "" ;       %% env var not set
             Opt ->              %% env var set to empty or others
               Opt
           end,

  ?DEBUG("Git clone Option = ~p",[Option]),
  Option.

