# rebar3\_elixir

A rebar3 plugin to **include Elixir dependencies** in your Erlang
application. This plugin **can handle transitive
dependencies**. Elixir dependencies can be specified as **Hex** or
**Git** dependencies.

[TOC]

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

The actual plugin version requires Rebar3 3.8.0 or above and Elixir 1.6 or above.

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

2. Specify Elixir dependencies from hex using the key `hex` (rebar3 syntax is kept for specifying Elixir dependencies from git):

```erlang
{deps, [
    {absinthe, {hex, "absinthe" ,"1.4.0"}},  %% An Elixir dependency in Hex
    {ecto, {git, "https://github.com/elixir-ecto/ecto.git"}}  %% A regular Elixir dependency in Git
]}.
```

## Example

We have prepared an interesting benchmark to use the plugin: directory
`example` contains an Erlang application that depends on several
Elixir libraries.

## Known problems

### Overriding a child dependency

- Configuration

```erlang
%%-*- mode: erlang -*-

{erl_opts, [debug_info]}.

{plugins, [
    {rebar3_elixir, ".*", {git, "https://github.com/Supersonido/rebar3_elixir.git", {branch, "feature/rebar_update"}}}
]}.

{deps,  [
         %% Some Erlang dependencies

         {poolboy, "1.5.2"}
        %,{metrics, {hex, "metrics", "2.5.0"}}
        %,{cowboy, "2.6.1"}

         %% Some Elixir dependencies

        %,{ex_aws, {hex, "ex_aws", "2.1.0"}}
        % ,{csv, {hex, "csv", "2.3.0"}}
        %,{ex_machina, {hex, "ex_machina", "2.3.0"}}
        % ,{absinthe, {hex, "absinthe", "1.4.16"}}
        %,{phoenix, {hex, "phoenix", "1.4.2"}}
        % ,{ecto, {hex, "ecto", "3.0.2"}}
        %,{poison, {hex, "poison", "3.1.0"}}
        ,{jason, {hex, "jason", "1.1.2"}}
        % ,{decimal, {hex, "decimal", "1.7.0"}}
        %,{exprotobuf, {hex, "exprotobuf", "1.2.9"}}
        ]}.

{shell, [
  % {config, "config/sys.config"},
    {apps, [example]}
]}.
```

- Scenario

```bash
angel@T440p: /example (feature/rebar_update)$ rm -rf _build && ./rebar3 unlock && ./rebar3 compile
===> Fetching rebar3_elixir ({git,"https://github.com/Supersonido/rebar3_elixir.git",
                                  {branch,"feature/rebar_update"}})
===> Compiling rebar3_elixir
===> Verifying dependencies...
===> Fetching jason ({hex,"jason","1.1.2"})
===> Fetching poolboy ({pkg,<<"poolboy">>,<<"1.5.2">>})
===> Version cached at /home/angel/.cache/rebar3/hex/hexpm/packages/poolboy-1.5.2.tar is up to date, reusing it
===> Compiling poolboy
===> Compiling jason
* Getting benchee_html (https://github.com/michalmuskala/benchee_html.git)
remote: Enumerating objects: 6, done.
remote: Counting objects: 100% (6/6), done.
remote: Compressing objects: 100% (6/6), done.
remote: Total 1385 (delta 0), reused 6 (delta 0), pack-reused 1379
Receiving objects: 100% (1385/1385), 1.42 MiB | 2.03 MiB/s, done.
Resolving deltas: 100% (600/600), done.
Resolving Hex dependencies...
Dependency resolution completed:
New:
  benchee 0.99.0
  decimal 1.7.0
  deep_merge 1.0.0
  dialyxir 0.5.1
  earmark 1.3.2
  ex_doc 0.19.3
  exjsx 4.0.0
  jiffy 0.15.2
  json 1.2.5
  jsone 1.4.7
  jsx 2.8.3
  makeup 0.8.0
  makeup_elixir 0.13.0
  nimble_parsec 0.5.0
  poison 3.1.0
  stream_data 0.4.2
  tiny 1.0.1
* Getting decimal (Hex package)
* Getting benchee (Hex package)
* Getting poison (Hex package)
* Getting exjsx (Hex package)
* Getting tiny (Hex package)
* Getting jsone (Hex package)
* Getting jiffy (Hex package)
* Getting json (Hex package)
* Getting dialyxir (Hex package)
* Getting ex_doc (Hex package)
* Getting stream_data (Hex package)
* Getting earmark (Hex package)
* Getting makeup_elixir (Hex package)
* Getting makeup (Hex package)
* Getting nimble_parsec (Hex package)
* Getting jsx (Hex package)
* Getting deep_merge (Hex package)
Dependencies have diverged:
* benchee_html (https://github.com/michalmuskala/benchee_html.git)
  the dependency benchee_html in mix.exs is overriding a child dependency:

  > In mix.exs:
    {:benchee_html, "~> 0.1", [env: :prod, git: "https://github.com/michalmuskala/benchee_html.git", only: :bench, manager: :mix]}

  > In deps/json/mix.exs:
    {:benchee_html, "~> 0.1", [only: :bench, env: :prod, hex: "benchee_html", repo: "hexpm", optional: true]}

  Ensure they match or specify one of the above in your deps and set "override: true"
** (Mix) Can't continue due to errors on dependencies
===> sh(mix deps.get)
failed with return code 1 and the following output:
* Getting benchee_html (https://github.com/michalmuskala/benchee_html.git)
remote: Enumerating objects: 6, done.
remote: Counting objects: 100% (6/6), done.
remote: Compressing objects: 100% (6/6), done.
remote: Total 1385 (delta 0), reused 6 (delta 0), pack-reused 1379
Receiving objects: 100% (1385/1385), 1.42 MiB | 2.03 MiB/s, done.
Resolving deltas: 100% (600/600), done.
Resolving Hex dependencies...
Dependency resolution completed:
New:
  benchee 0.99.0
  decimal 1.7.0
  deep_merge 1.0.0
  dialyxir 0.5.1
  earmark 1.3.2
  ex_doc 0.19.3
  exjsx 4.0.0
  jiffy 0.15.2
  json 1.2.5
  jsone 1.4.7
  jsx 2.8.3
  makeup 0.8.0
  makeup_elixir 0.13.0
  nimble_parsec 0.5.0
  poison 3.1.0
  stream_data 0.4.2
  tiny 1.0.1
* Getting decimal (Hex package)
* Getting benchee (Hex package)
* Getting poison (Hex package)
* Getting exjsx (Hex package)
* Getting tiny (Hex package)
* Getting jsone (Hex package)
* Getting jiffy (Hex package)
* Getting json (Hex package)
* Getting dialyxir (Hex package)
* Getting ex_doc (Hex package)
* Getting stream_data (Hex package)
* Getting earmark (Hex package)
* Getting makeup_elixir (Hex package)
* Getting makeup (Hex package)
* Getting nimble_parsec (Hex package)
* Getting jsx (Hex package)
* Getting deep_merge (Hex package)
Dependencies have diverged:
* benchee_html (https://github.com/michalmuskala/benchee_html.git)
  the dependency benchee_html in mix.exs is overriding a child dependency:

  > In mix.exs:
    {:benchee_html, "~> 0.1", [env: :prod, git: "https://github.com/michalmuskala/benchee_html.git", only: :bench, manager: :mix]}

  > In deps/json/mix.exs:
    {:benchee_html, "~> 0.1", [only: :bench, env: :prod, hex: "benchee_html", repo: "hexpm", optional: true]}

  Ensure they match or specify one of the above in your deps and set "override: true"
** (Mix) Can't continue due to errors on dependencies

angel@T440p: /example (feature/rebar_update)$
```
