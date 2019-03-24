-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  State1 = rebar3_elixir_utils:add_elixir(State),
  State2 = rebar_state:add_resource(State1, {iex, rebar3_elixir_hex}),
  State3 = rebar_state:add_resource(State2, {iex_dep, rebar3_elixir_dep}),
  State4 =
    if erlang:function_exported(rebar_state, add_project_builder, 3) -> 
        rebar_state:add_project_builder(State3, mix, rebar3_elixir_compiler);
       true -> State3
    end,
  State5 = rebar3_elixir_utils:add_elixir_to_path(State4),
  {ok, State5}.
