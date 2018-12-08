rebar3_elixir
=====

Rebar3_elixir is a plugin that allows you to compile applications written in elixir as dependencies of erlang applications.
You are able to compile dependencies from hex.pm resource and elixir code which is host in git repositories.


Usage
-----

An example of rebar.config is:

```erlang
{erl_opts, [debug_info]}.

%% The list of dependencies
{deps, [
    {faker, {iex, "faker" ,"0.6.0"}}  %% hex.pm dependency
    {ecto, {iex_git, "https://github.com/elixir-ecto/ecto.git"}}  %% git dependency
    %% or
    {ecto, {iex_git, "https://github.com/elixir-ecto/ecto.git", {branch, "master"}}}  %% git dependency
]}.

%% The list of plugins
{plugins, [
    {rebar3_elixir, ".*", {git, "https://github.com/Supersonido/rebar3_elixir.git", {branch, "master"}}}
]}.

%% Config the provider hooks
{provider_hooks, [
    {pre, [{compile, {iex, rebar3_elixir}}]}
]}.

```


Directory structure
--------
The plugin creates `./_elixir_build` to fetch and compile your dependencies specified in rebar.config.


Warning
-----
The use of this plugin in production environments will be under your responsibility.
