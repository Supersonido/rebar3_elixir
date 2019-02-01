-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  State1 = rebar3_elixir_utils:add_elixir(State),
  State2 = rebar_state:add_resource(State1, {iex, rebar3_elixir_hex}),
  State3 = rebar_state:add_project_builder(State2, mix, rebar3_elixir_compiler),
  {ok, State3}.
