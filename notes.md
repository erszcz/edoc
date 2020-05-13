# 2020-03-28

A `@deprecated` tag might look like this:

```
%% @deprecated See {@link file/2} for details.
```

When handled with:

```
Deprecated = extract_doc_contents("./deprecated/description/fullDescription", Doc, Opts),
```

it is placed in the chunk as:

```
#{deprecated =>
      [<<"See ">>,
       {a,[{href,<<"#file-2">>}],[{code,[],[<<"file/2">>]}]},
       <<" for details.">>],
```

which is not conformat with EEP-48 which requires a binary.

When handled with:

```
Deprecated = xpath_to_text("./deprecated/description/fullDescription", Doc, Opts),
```

Then it's output to chunk with Markdown syntax:

```
#{deprecated => <<"See `file/2` for details.">>,
```

# 2020-03-31

Check tags:

```
$ ERL_LIBS=/Users/erszcz/work/erszcz/kvs/_checkouts/rebar3_edoc_chunks/_build/default/lib/ ~/work/erszcz/kvs/_checkouts/rebar3_edoc_chunks/priv/bin/edoc.escript -chunks -pa /Users/erszcz/work/erszcz/recon/_build/default/lib/recon/ebin -app recon
Running with opts:
#{app => recon,
  code_paths =>
      ["/Users/erszcz/work/erszcz/recon/_build/default/lib/recon/ebin"],
  files => [],mode => chunks,run => app}
edoc: warning: 'tt' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
```

# 2020-04-01

## Check more tags

```
09:54:45 erszcz @ x5 : ~/work/erszcz/edoc (wip)
$ ERL_LIBS=/Users/erszcz/work/erszcz/kvs/_checkouts/rebar3_edoc_chunks/_build/default/lib/ ~/work/erszcz/kvs/_checkouts/rebar3_edoc_chunks/bin/edoc.escript -chunks -pa _build/default/lib/edoc/ebin/ -app edoc
Running with opts:
#{app => edoc,
  code_paths => ["_build/default/lib/edoc/ebin/"],
  files => [],mode => chunks,run => app}
_build/default/lib/edoc/include/../include/edoc_doclet.hrl: warning: documentation before module declaration is ignored by @headerfile
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
edoc: warning: 'expr' is not accepted - skipping tag, extracting content
edoc: warning: 'see' is not accepted - skipping tag, extracting content
_build/default/lib/edoc/src/edoc_types.hrl: warning: documentation before module declaration is ignored by @headerfile
_build/default/lib/edoc/include/../include/edoc_doclet.hrl: warning: documentation before module declaration is ignored by @headerfile
_build/default/lib/edoc/include/../include/edoc_doclet.hrl: warning: documentation before module declaration is ignored by @headerfile
edoc: warning: 'strong' is not accepted - skipping tag, extracting content
```

## Running `ex_doc` on EDoc

```
$ /Users/erszcz/work/elixir-lang/ex_doc/ex_doc edoc "0.11" _build/default/lib/edoc/ebin --main edoc
** (MatchError) no match of right hand side value: nil
    (ex_doc 0.21.2) lib/ex_doc/retriever.ex:486: ExDoc.Retriever.get_type/4
    (ex_doc 0.21.2) lib/ex_doc/retriever.ex:477: anonymous fn/5 in ExDoc.Retriever.get_types/2
    (elixir 1.10.2) lib/enum.ex:2111: Enum."-reduce/3-lists^foldl/2-0-"/3
    (ex_doc 0.21.2) lib/ex_doc/retriever.ex:476: ExDoc.Retriever.get_types/2
    (ex_doc 0.21.2) lib/ex_doc/retriever.ex:205: ExDoc.Retriever.do_generate_node/3
    (ex_doc 0.21.2) lib/ex_doc/retriever.ex:191: ExDoc.Retriever.generate_node/3
    (elixir 1.10.2) lib/enum.ex:3343: Enum.flat_map_list/2
    (ex_doc 0.21.2) lib/ex_doc/retriever.ex:43: ExDoc.Retriever.docs_from_modules/2
```

This seems to be caused by types in docs chunk and types in the AST not matching:

  - AST:

    ```
    > {ok, {edoc, [{_, BDbgi}]}} = beam_lib:chunks(code:which(edoc), ["Dbgi"]).
    > {debug_info_v1, _, {Forms, _}} = binary_to_term(BDbgi).
    > Types = [ Attr || Attr = {attribute, _, type, _} <- Forms ].
    [{attribute,65,type,{edoc_module,{type,65,any,[]},[]}}]
    ```

  - docs:

    ```
    > {ok, Docs} = code:get_doc(edoc).
    > [ Type || {{type, _, _} = Type, _, _, _, _} <- Docs#docs_v1.docs ].
    [{type,edoc_module,0},
     {type,filename,0},
     {type,proplist,0},
     {type,comment,0},
     {type,syntaxTree,0}]
    ```

## Docs missing

See file:///Users/erszcz/work/erszcz/edoc/doc/edoc.html for file/2 doc.
The documentation is truncated at some arbitrary point - figure this out.

# 2020-04-04

## maps:map(..., none)

```
32> I0.
[0|#{}]
33> maps:next(I0).
none
34> maps:map(fun (_,_) -> ok end, maps:next(I0)).
#{}
35> maps:map(fun (_,_) -> ok end, none).
#{}
36> maps:map(fun (_,_) -> ok end, z).
** exception error: {badmap,z}
     in function  maps:map/2
        called as maps:map(#Fun<erl_eval.12.128620087>,z)
```


## @private and @hidden on module level

EDoc `@doc`, chunk `#{lang() := doc()}`:

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
f() -> ok.

%% @doc asd
main(Args) ->
```

```
3> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,
             #{<<"en">> => [<<"EDoc command line interface">>]},
             #{},
             [{{function,f,0},
               0,
               [<<"f/0">>],
               #{<<"en">> => [<<"Test function.">>]},
               #{}},
              {{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}}]}}
```

EDoc `@private`, chunk `hidden`:

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
%% @private
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
f() -> ok.

%% @doc asd
```

```
5> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,hidden,
             #{},
             [{{function,f,0},
               0,
               [<<"f/0">>],
               #{<<"en">> => [<<"Test function.">>]},
               #{}},
              {{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}}]}}
```

EDoc `@hidden`, chunk `none`:

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
%% @hidden
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
f() -> ok.

%% @doc asd
```

```
7> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,none,
             #{},
             [{{function,f,0},
               0,
               [<<"f/0">>],
               #{<<"en">> => [<<"Test function.">>]},
               #{}},
              {{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}}]}}
```


## @private and @hidden on function level

EDoc `@doc`, chunk `#{lang() := doc()}`:

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
f() -> ok.

%% @doc asd
main(Args) ->
```

```
9> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,
             #{<<"en">> => [<<"EDoc command line interface">>]},
             #{},
             [{{function,f,0},
               0,
               [<<"f/0">>],
               #{<<"en">> => [<<"Test function.">>]},
               #{}},
              {{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}}]}}
```

EDoc `@private`, no `f/0` in the chunk!

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
%% @private
f() -> ok.

%% @doc asd
```

```
11> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,
             #{<<"en">> => [<<"EDoc command line interface">>]},
             #{},
             [{{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}}]}}
```

Trace reveals that the function is not passed at the xmerl level:

```
(search)`:files': edoc:files(["src/edoc_cli.erl"], [{doclet, edoc_doclet_chunks}, {layout, edoc_layout_chunks}]).
(<0.155.0>) call edoc_layout_chunks:edoc_to_chunk({xmlElement,module,[],[],
    {xmlNamespace,[],[]},
    [],1,
    [{xmlAttribute,name,[],[],[],[{module,1}],1,[],"edoc_cli",undefined},
     {xmlAttribute,root,[],[],[],[{module,1}],2,[],[],undefined},
     {xmlAttribute,encoding,[],[],[],[{module,1}],3,[],"utf8",undefined}],
    [{xmlElement,description,[],[],
         {xmlNamespace,[],[]},
         [{module,1}],
         1,[],
         [{xmlElement,briefDescription,[],[],
              {xmlNamespace,[],[]},
              [{description,1},{module,1}],
              1,[],
              [{xmlText,
                   [{briefDescription,1},{description,1},{module,1}],
                   1,[],"EDoc command line interface.",text}],
              [],[],undeclared},
          {xmlElement,fullDescription,[],[],
              {xmlNamespace,[],[]},
              [{description,1},{module,1}],
              2,[],
              [{xmlText,
                   [{fullDescription,2},{description,1},{module,1}],
                   1,[],"EDoc command line interface",text}],
              [],[],undeclared}],
         [],[],undeclared},
     {xmlElement,typedecls,[],[],
         {xmlNamespace,[],[]},
         [{module,1}],
         2,[],[],[],[],undeclared},
     {xmlElement,functions,[],[],
         {xmlNamespace,[],[]},
         [{module,1}],
         3,[],
         [{xmlElement,function,[],[],
              {xmlNamespace,[],[]},
              [{functions,3},{module,1}],
              1,
              [{xmlAttribute,name,[],[],[],
                   [{function,1},{functions,3},{module,1}],
                   1,[],"main",undefined},
               {xmlAttribute,arity,[],[],[],
                   [{function,1},{functions,3},{module,1}],
                   2,[],"1",undefined},
               {xmlAttribute,exported,[],[],[],
                   [{function,1},{functions,3},{module,1}],
                   3,[],"yes",undefined},
               {xmlAttribute,label,[],[],[],
                   [{function,1},{functions,3},{module,1}],
                   4,[],"main-1",undefined}],
              [{xmlElement,args,[],[],
                   {xmlNamespace,[],[]},
                   [{function,1},{functions,3},{module,1}],
                   1,[],
                   [{xmlElement,arg,[],[],
                        {xmlNamespace,[],[]},
                        [{args,1},{function,1},{functions,3},{module,1}],
                        1,[],
                        [{xmlElement,argName,[],[],
                             {xmlNamespace,[],[]},
                             [{arg,1},
                              {args,1},
                              {function,1},
                              {functions,3},
                              {module,1}],
                             1,[],
                             [{xmlText,
                                  [{argName,1},
                                   {arg,1},
                                   {args,1},
                                   {function,1},
                                   {functions,3},
                                   {module,1}],
                                  1,[],"Args",text}],
                             [],[],undeclared}],
                        [],[],undeclared}],
                   [],[],undeclared},
               {xmlElement,description,[],[],
                   {xmlNamespace,[],[]},
                   [{function,1},{functions,3},{module,1}],
                   2,[],
                   [{xmlElement,briefDescription,[],[],
                        {xmlNamespace,[],[]},
                        [{description,2},
                         {function,1},
                         {functions,3},
                         {module,1}],
                        1,[],
                        [{xmlText,
                             [{briefDescription,1},
                              {description,2},
                              {function,1},
                              {functions,3},
                              {module,1}],
                             1,[],"asd.",text}],
                        [],[],undeclared},
                    {xmlElement,fullDescription,[],[],
                        {xmlNamespace,[],[]},
                        [{description,2},
                         {function,1},
                         {functions,3},
                         {module,1}],
                        2,[],
                        [{xmlText,
                             [{fullDescription,2},
                              {description,2},
                              {function,1},
                              {functions,3},
                              {module,1}],
                             1,[],"asd",text}],
                        [],[],undeclared}],
                   [],[],undeclared}],
              [],[],undeclared}],
         [],[],undeclared}],
    [],[],undeclared},[{doclet,edoc_doclet_chunks},{layout,edoc_layout_chunks}])
(<0.155.0>) returned from edoc_layout_chunks:edoc_to_chunk/2 -> {docs_v1,0,
                                                                 erlang,
                                                                 <<"application/erlang+html">>,
                                                                 #{<<"en">> =>
                                                                    [<<"EDoc command line interface">>]},
                                                                 #{},
                                                                 [{{function,
                                                                    main,1},
                                                                   0,
                                                                   [<<"main/1">>],
                                                                   #{<<"en">> =>
                                                                      [<<"asd">>]},
                                                                   #{}}]}
```

EDoc `@hidden`, no `f/0` in the chunk!

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
%% @hidden
f() -> ok.

%% @doc asd
```

```
19> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,
             #{<<"en">> => [<<"EDoc command line interface">>]},
             #{},
             [{{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}}]}}
```

Trace reveals the same as with `@private` on a function - the function is not passed
on the xmerl level.

When `edoc:get_doc(File, Env, [private, hidden | Options])`:

```
$ head src/edoc_cli.erl
%% @doc EDoc command line interface
-module(edoc_cli).
-export([f/0,
	 main/1]).

%% @doc Test function.
%% @private
f() -> ok.

%% @doc asd
```

```
3> code:get_doc(edoc_cli).
{ok,{docs_v1,0,erlang,<<"application/erlang+html">>,
             #{<<"en">> => [<<"EDoc command line interface">>]},
             #{},
             [{{function,f,0},
               0,
               [<<"f/0">>],
               #{<<"en">> => [<<"Test function.">>]},
               #{}},
              {{function,main,1},
               0,
               [<<"main/1">>],
               #{<<"en">> => [<<"asd">>]},
               #{}},
              '...']}}
```

In other words, `@private` tag is not forwarded on the xmerl level, though
the function entry is present.
This filtering is done by `edoc_data:hidden_filter/2`.
Data for this filter is passed in in options (`private` and `hidden`),
as well as recorded per entry in `#entry{}`.

This probably could be enabled by extending the `#entry{}` record with
`private` and `hidden` fields. See also `edoc_extract:collect/8`.

%% TODO: @private and @hidden info could be forwarded on the xmerl level
in `edoc_data:function/6`.

# 2020-05-07

ToDo:

- [x] function signature: Name/Arity
- [ ] type signature: -spec Type() :: Def.
