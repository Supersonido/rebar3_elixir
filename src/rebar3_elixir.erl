-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  State1 = rebar3_elixir_utils:add_elixir(State),
  State2 = rebar_state:add_resource(State1, {hex, rebar3_elixir_hex}),
  State3 = rebar_state:add_resource(State2, {iex_dep, rebar3_elixir_dep}),
  State4 =
    case erlang:function_exported(rebar_state, add_project_builder, 3) of
      true -> rebar_state:add_project_builder(State3, mix, rebar3_elixir_compiler);
      _ -> State3
    end,
  State5 = rebar3_elixir_utils:add_elixir_to_path(State4),

  LibDir = rebar3_elixir_utils:get_lib_dir(State5),
  RelxConfig = rebar_state:get(State5, relx, []),
  NewRelxConfig = 
    case lists:keyfind(lib_dirs, 1, RelxConfig) of
      {lib_dirs, OldLibDir} -> 
        NewLibDir = OldLibDir ++ [LibDir],
        lists:keyreplace(lib_dirs, 1, RelxConfig, {lib_dirs, NewLibDir});
      false -> 
        [{lib_dirs, [LibDir]}] ++ RelxConfig  
    end,
  State6 = rebar_state:set(State4, relx, NewRelxConfig),  
  {ok, State6}.
