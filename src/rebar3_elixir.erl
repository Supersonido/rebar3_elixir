-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  State1 = rebar3_elixir_utils:add_elixir(State),
  State2 = rebar_state:add_resource(State1, {iex, rebar3_elixir_hex}),
  State3 = rebar_state:add_resource(State2, {iex_dep, rebar3_elixir_dep}),
  
  
  State4 =
    try  of
        rebar_state:add_project_builder(State3, mix, rebar3_elixir_compiler)
    catch
      _ -> State3
    end,
  
  State5 = rebar3_elixir_utils:add_elixir_to_path(State4),
  {ok, State5}.
