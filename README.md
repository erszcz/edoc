# Archived - merged back into Erlang/OTP

This repository is now archived as [the work done here was merged upstream into Erlang/OTP and became part of OTP 24](https://github.com/erlang/otp/pull/2803).

Thanks to that it's now possible to [generate ExDoc documentation for Erlang projects](https://github.com/elixir-lang/ex_doc#using-exdoc-with-erlang-projects). This is already happening in the community as evidenced by projects such as [`rebar3_hex`](https://hexdocs.pm/rebar3_hex/readme.html), but also [`telemetry`](https://hexdocs.pm/telemetry/readme.html), [`systemd`](https://hexdocs.pm/systemd), [`ram`](https://hexdocs.pm/ram/readme.html), or [`syn`](https://hexdocs.pm/syn/readme.html) to name a few.

The same mechanism - [`docs` chunk defined by EEP-48](https://www.erlang.org/eeps/eep-0048) - also provides the [`shell_docs`](https://www.erlang.org/doc/man/shell_docs.html) documentation for non-OTP Erlang projects.

# edoc

[![TravisCI Build Status](https://travis-ci.org/erszcz/edoc.svg?branch=master)](https://travis-ci.org/erszcz/edoc)

OTP 23.0 ships with [EEP-48](https://github.com/erlang/eep/blob/master/eeps/eep-0048.md) support
and [online help in the shell](http://blog.erlang.org/OTP-23-Highlights/).
This support is based on doc chunks, a new format for storing Erlang module documentation.
This fork of EDoc emits doc chunks for non-OTP projects.
It means online help will be available for all Erlang projects using EDoc.

The expected end of life of this project is merging back into OTP once the
changes are polished enough. See
https://github.com/erlef/documentation-wg/issues/4 for progress reports.


## Build

```
$ rebar3 compile
```


## Use

You can generate doc chunks for your project in two ways: via a command line script
or by using a Rebar3 plugin.


### Make doc chunks - CLI

```
ERL_LIBS=/Users/erszcz/work/erszcz/edoc/_build/default/lib/ \
    bin/edoc.escript -chunks -pa _build/default/lib/edoc/ebin/ -app edoc
```

`ERL_LIBS` is necessary for the out-of-OTP EDoc to be first in the Erlang code path.
Otherwise, OTP-shipped EDoc with no chunks support would be used.

### Make doc chunks - Rebar3 plugin

Put this in your `rebar.config`:

```
{plugins,
 [
  {rebar3_edoc_chunks, {git, "https://github.com/erszcz/edoc.git", {branch, "wip"}}}
 ]}.

{provider_hooks,
 [
  {post, [{compile, {edoc_chunks, compile}}]}
 ]}.
```

Then just `rebar3 compile` and find the chunks under
`_build/default/lib/$PROJECT/doc/chunks`.

See https://github.com/erszcz/kvs/blob/4feb22b5397e2ffa620dab15e72e84893d9de8ef/rebar.config
for a working `rebar.config` example using profiles.

### Generate ExDoc HTML documentation

It's also possible to use an experimental branch of ExDoc to generate HTML
docs from the chunks.
Build ExDoc and then:

```
/Users/erszcz/work/elixir-lang/ex_doc/ex_doc \
    edoc "0.11" _build/default/lib/edoc/ebin --main edoc
```

Please note that as of now
[`wm-erlang` ExDoc branch is required](https://github.com/elixir-lang/ex_doc/tree/wm-erlang).
