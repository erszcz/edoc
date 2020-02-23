{ok, {_, [{_, BDocs}]}} = beam_lib:chunks(code:which(edoc), ["Docs"]).
Docs = erlang:binary_to_term(BDocs).
rr(edoc_chunks).
Entries = Docs#docs_v1.docs.
[ E || {{type, _, _}, _, _, _, _} = E <- Entries ].

%% [{{type,filename,0},0,[<<"filename/0">>],none,#{}},
%%  {{type,proplist,0},0,[<<"proplist/0">>],none,#{}},
%%  {{type,comment,0},0,[<<"comment/0">>],none,#{}},
%%  {{type,syntaxTree,0},0,[<<"syntaxTree/0">>],none,#{}},
%%  {{type,edoc_module,0},
%%   0,
%%   [<<"edoc_module/0">>],
%%   #{<<"en">> =>
%%         <<"The EDoc documentation data for a module,\nexpressed as an XML document in XM"...>>},
%%   #{}}]
%% > % {{:type, :filename, 0}, 0, ["filename/0"], :none, %{}}

{ok, {_, [{_, BDbgi}]}} = beam_lib:chunks(code:which(edoc), ["Dbgi"]).
Dbgi = erlang:binary_to_term(BDbgi).
{_, _, {AST, _}} = Dbgi = erlang:binary_to_term(BDbgi).
[ Form || {attribute, _, _, _} = Form <- AST ].

f().
edoc:files(["src/edoc.erl"], [{doclet, edoc_doclet_chunks}, {dir, "doctest"}]).
{ok, BChunk} = file:read_file("doctest/chunks/edoc.chunk").
Chunk = binary_to_term(BChunk).

f().
edoc:files(["src/edoc.erl"], [{doclet, edoc_doclet_chunks}, {dir, "doctest"}, {layout, edoc_layout_chunk_htmltree}]).
{ok, BChunk} = file:read_file("doctest/chunks/edoc.chunk").
Chunk = binary_to_term(BChunk).

f().
edoc:files(["src/edoc.erl"], [{doclet, edoc_doclet_chunks}, {dir, "doctest"},
			      {layout, edoc_layout_chunk_htmltree}]).
{ok, BChunk} = file:read_file("doctest/chunks/edoc.chunk").
Chunk = binary_to_term(BChunk).

f(ReadChunk).
ReadChunk = fun (File) ->
                    {ok, BChunk} = file:read_file(File),
                    Chunk = binary_to_term(BChunk)
            end.
ReadChunk("doctest/recon-chunks/chunks/recon.chunk").
ReadChunk("doctest/recon-markdown-chunks/chunks/recon.chunk").
ReadChunk("doctest/stdlib-chunks/chunks/ets.chunk").

%% This fails on missing logger.hrl...
edoc:application(recon, [{doclet, edoc_doclet_chunks},
                         {layout, edoc_layout_chunk_htmltree},
                         {preprocess, true},
                         {includes, ["/Users/erszcz/.asdf/installs/erlang/21.1/lib/kernel-6.1/include",
                                     "/Users/erszcz/.asdf/installs/erlang/21.1/lib/stdlib-3.6/include/"]},
                         {dir, "doctest/recon-chunks"}]).

%% ...but this fails on missing logger.hrl, too,
%% so it's not a regression.
%% Simply, edoc:application() doesn't handle `include` paths
%% based on `.app` contents.
edoc:application(stdlib, [{preprocess, true},
                          {includes, ["/Users/erszcz/.asdf/installs/erlang/21.1/lib/kernel-6.1/include"]},
                          {dir, "doctest/stdlib-default"}]).

edoc:application(recon, [{doclet, edoc_doclet_chunks},
                         {layout, edoc_layout_chunk_markdown},
                         {preprocess, true},
                         {includes, ["/Users/erszcz/.asdf/installs/erlang/21.1/lib/kernel-6.1/include",
                                     "/Users/erszcz/.asdf/installs/erlang/21.1/lib/stdlib-3.6/include/"]},
                         {dir, "doctest/recon-markdown-chunks"}]).

dbg:stop_clear().
dbg:tracer().
dbg:p(all, call).
dbg:tpl(edoc, run, x).
%dbg:tpl(edoc_doclet_default, source, []).
dbg:tpl(edoc_doclet_chunks, options_with_defaults, x).
%dbg:tpl(edoc_lib, run_layout, x).
%dbg:tpl(edoc_lib, run_plugin, x).
