# edoc

[![TravisCI Build Status](https://travis-ci.org/erszcz/edoc.svg?branch=master)](https://travis-ci.org/erszcz/edoc)

EDoc OTP library extracted from [Erlang/OTP](https://github.com/erlang/otp).
The aim of this fork is to make EDoc emit `Docs` chunks.


## Build

```
$ rebar3 compile
```


## Use

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

### Run ExDoc

Build and run ExDoc:

```
/Users/erszcz/work/elixir-lang/ex_doc/ex_doc \
    edoc "0.11" _build/default/lib/edoc/ebin --main edoc
```

Please note that as of now
[`wm-erlang` ExDoc branch is required](https://github.com/elixir-lang/ex_doc/tree/wm-erlang).
