# edoc

[![TravisCI Build Status](https://travis-ci.org/erszcz/edoc.svg?branch=master)](https://travis-ci.org/erszcz/edoc)

EDoc OTP library extracted from [Erlang/OTP](https://github.com/erlang/otp).
The aim of this fork is to make EDoc emit `Docs` chunks.


## Build

```
$ rebar3 compile
```


## Use

Put this in your `rebar.config`:

```
{plugins,
 [
  {rebar3_edoc_chunks, {git, "https://github.com/erszcz/edoc.git", {branch, "import-docs-chunks"}}}
 ]}.

{provider_hooks,
 [
  {post, [{compile, {edoc_chunks, compile}}]}
 ]}.
```

Then build and run ExDoc:

```
rebar3 compile
/Users/erszcz/work/elixir-lang/ex_doc/ex_doc \
    edoc 0.12 _build/default/lib/edoc/ebin -m edoc -o docs
```

Please note that as of now
[`wm-erlang` ExDoc branch is required](https://github.com/elixir-lang/ex_doc/tree/wm-erlang).
