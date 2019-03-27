# rebar3\_elixir

A rebar3 plugin to **include Elixir dependencies** in your Erlang
application. This plugin **can handle transitive
dependencies**. Elixir dependencies can be specified as **Hex** or
**Git** dependencies.

## Disclaimer

The plugin hasn't been tested with a relevant range of Elixir
dependencies so I assume that those Elixir libraries that makes
extensive use of metaprogramming might not work properly.  The use of
this plugin in production environments will be **under your
responsibility.**

## The transitivity problem

### Erlang-Elixir-Whatever

The most popular plugin for including Elixir dependencies in Erlang applications,
[`rebar_mix`](https://github.com/tsloughter/rebar_mix), presents an
important limitation for us: do not compile transitive
dependencies. In other words, if you include an Elixir dependency _A_ and _A_
depends on _B_ then _B_ is not compiled by `rebar_mix`.

Rebar3 3.7.0 allows specifying Elixir dependencies (Hex and Git
dependencies) but cannot handle transitive dependencies either.

### Elixir-Erlang-Whatever

At this moment, Mix 1.8 cannot handle Erlang dependencies that depends
on Elixir applications since it assumes that Erlang applications just have
Erlang dependencies. I have [a fork of
Mix](https://github.com/Supersonido/elixir) (branch v1.6) where this
limitation is overcome. A PR is on the way.

## Requirements

The actual plugin version requires Rebar3 3.7.0 or above and Elixir 1.6 or above.

If you have an older version of Rebar3, you can use the
version v0.1.0 of this plugin and check the README.

## Usage

Edit your `rebar.config` and follow these instructions:

1. Add the plugin:

```erlang
{plugins, [
    {rebar3_elixir, ".*", {git, "https://github.com/Supersonido/rebar3_elixir.git", {branch, "master"}}}
]}.
```

2. Specify Elixir dependencies from hex using the key `iex` (rebar3 syntax is kept for specifying Elixir dependencies from git):

```erlang
{deps, [
    {absinthe, {hex, "absinthe" ,"1.4.0"}},  %% An Elixir dependency in Hex
    {ecto, {git, "https://github.com/elixir-ecto/ecto.git"}}  %% A regular Elixir dependency in Git
]}.
```
