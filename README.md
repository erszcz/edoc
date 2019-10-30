# edoc

[![TravisCI Build Status](https://travis-ci.org/erszcz/edoc.svg?branch=master)](https://travis-ci.org/erszcz/edoc)

[Erlang/OTP](https://github.com/erlang/otp) EDoc fork to facilitate work
on emitting `Docs` chunks.

## Status

`Docs` chunks are written to disk.
As of now the functionality is based on code imported
from [`docsh`](https://github.com/erszcz/docsh),
but an alternative path might be taken, for example based
on [`docs_chunks`](https://github.com/wojtekmach/docs_chunks).

## Usage

```sh
$ rebar3 shell
```

Once in the Erlang shell:

```
%% Run EDoc to write the chunk file under `doctest-1`
edoc:files(["src/edoc.erl"], [{dir, "doctest-1"}, {doclet, edoc_doclet_eep48}]).

%% Read the chunk and display it
{ok, BDocs} = file:read_file("doctest-1/edoc.docs.chunk").
rp( erlang:binary_to_term(BDocs) ).
```
