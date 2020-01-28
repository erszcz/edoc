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
