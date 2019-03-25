rebar3\_elixir
=====
Rebar3\_elixir is a plugin that allows you to compile applications and libraries written in elixir, wherever is hosted (actually only Hex.pm and git), 
as dependencies of erlang applications. The plugin allows transitive dependencies.

Requirements
-----
The actual plugin version requires rebar3 3.7.0 or above and elixir 1.6 or above.

If you would like to use an older version of rebar3, you must use the version v0.1.0 of the plugin and check the README of these version
to check the correct way using it.

Usage
-----
You have to define some things inside the `rebar.config` file to use the plugin.

First of all, define the plugin:

```erlang
{plugins, [
    {rebar3_elixir, ".*", {git, "https://github.com/Supersonido/rebar3_elixir.git", {branch, "master"}}}
]}.
```

After that, define the elixir dependencies that you want to use (for the time being is advisable to use git as a dependency source):

```erlang
{deps, [
    {faker, {iex, "faker" ,"0.6.0"}},  %% hex.pm dependency
    {ecto, {git, "https://github.com/elixir-ecto/ecto.git"}}  %% as a regular git dependency
]}.
```

A full example would be:

```erlang
{erl_opts, [debug_info]}.

%% The list of dependencies
{deps, [
    {faker, {iex, "faker" ,"0.6.0"}},  %% hex.pm dependency
    {ecto, {git, "https://github.com/elixir-ecto/ecto.git"}}  %% git dependency
]}.

%% The list of plugins
{plugins, [
    {rebar3_elixir, ".*", {git, "https://github.com/Supersonido/rebar3_elixir.git", {branch, "master"}}}
]}.

```

Warning
-----
The use of this plugin in production environments will be under your responsibility.

Clarification
-----
At the moment, with the current version of mix, it is not allowed to have within an elixir application an erlang type dependency which
contains an elixir type dependency because they assume that an erlang application can only have erlang dependencies. 
I have a fork in my profile (https://github.com/Supersonido/elixir) in branch v1.6 in which I correct this assumption, 
we are trying to make it part of the official elixir repository.
