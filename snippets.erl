edoc:files(["src/edoc.erl"]).
edoc:files(["src/edoc.erl"], [{dir, "doctest-1"}]).
edoc:files(["src/edoc.erl"], [{dir, "doctest-1"}, {doclet, edoc_doclet_default}]).

f().
edoc:files(["src/edoc.erl"], [{dir, "doctest-1"}, {doclet, edoc_doclet_eep48}]).
{ok, BDocs} = file:read_file("doctest-1/edoc.docs.chunk").
rp( erlang:binary_to_term(BDocs) ).
