-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  {ok, State0} = rebar3_elixir_compiler:init(State),
  State1 = rebar3_elixir_utils:add_elixir(State0),
  State2 = rebar_state:add_resource(State1, {iex_git, rebar3_elixir_git}),
  State3 = rebar_state:add_resource(State2, {iex, rebar3_elixir_hex}),
  State4 = rebar_state:add_resource(State3, {iex_dep, rebar3_elixir_dep}),
  {ok, State4}.
