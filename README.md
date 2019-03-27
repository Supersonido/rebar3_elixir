# rebar3\_elixir

A rebar3 plugin to **include Elixir dependencies** in your Erlang
application. This plugin **can handle transitive
dependencies**. Elixir dependencies can be specified as **Hex** or
**Git**.

## The transitivity problem

### Erlang-Elixir-Whatever

The most popular plugin for including Elixir dependencies,
[`rebar_mix`](https://github.com/tsloughter/rebar_mix), presents an
important limitation for us: do not compile transitive
dependencies. I.e. if you include an Elixir dependency _A_ and _A_
depends on _B_ then _B_ is not compiled by `rebar_mix`.

Rebar3 3.7.0 allows specifying Elixir dependencies (Hex and Git
dependencies) but cannot handle transitive dependencies.

### Elixir-Erlang-Whatever

At this moment, Mix 1.8 cannot handle Erlang dependencies that depends
on Elixir applications. Mix assume that Erlang applications just have
Erlang dependencies. I have [a fork of
Mix](https://github.com/Supersonido/elixir) (branch v1.6) where this
limitation is overcome. A PR is on the way.

## Requirements

The actual plugin version requires rebar3 3.7.0 or above and elixir 1.6 or above.

If you would like to use an older version of rebar3, you must use the
version v0.1.0 of the plugin and check the README of that version.

## Usage

Edit your `rebar.config` following these instructions:

1. Add the plugin:

```erlang
{plugins, [
    {rebar3_elixir, ".*", {git, "https://github.com/Supersonido/rebar3_elixir.git", {branch, "master"}}}
]}.
```

2. Specify Elixir dependencies from hex using the key `iex` (rebar3 syntax is kept for specifying Elixir dependencies from git):

```erlang
{deps, [
    {faker, {iex, "faker" ,"0.6.0"}},  %% hex.pm dependency
    {ecto, {git, "https://github.com/elixir-ecto/ecto.git"}}  %% as a regular git dependency
]}.
```

## Disclaimer

The use of this plugin in production environments will be under your responsibility.
