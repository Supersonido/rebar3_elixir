-module(rebar3_elixir).

-export([init/1]).

init(State) ->
  {ok, State0} = rebar3_elixir_compiler:init(State),
  rebar3_elixir_utils:add_elixir(State),
  State1 = rebar_state:add_resource(State0, {iex_git, rebar3_elixir_git}),
  State2 = rebar_state:add_resource(State1, {iex, rebar3_elixir_hex}),
  {ok, State2}.
