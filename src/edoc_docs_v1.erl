-module(edoc_docs_v1).

-export_type([t/0,
              entry/0,
              beam_language/0,
              mime_type/0,
              doc/0,
              doc_language/0,
              doc_string/0,
              metadata/0,
              signature/0]).

%% @type t(). The Docs v1 chunk according to EEP 48.
-type t() :: #docs_v1{anno :: erl_anno:anno(),
		      beam_language :: beam_language(),
		      format :: mime_type(),
		      module_doc :: doc(),
		      metadata :: metadata(),
		      docs :: [entry()]}.

-type entry() :: #docs_v1_entry{kind_name_arity :: {atom(), atom(), arity()},
				anno :: erl_anno:anno(),
				signature :: signature(),
				doc :: doc(),
				metadata :: metadata()}.

-type beam_language() :: atom().
-type mime_type() :: binary().
-type doc() :: #{doc_language() => doc_string()} | none | hidden.
-type doc_language() :: binary().
-type doc_string() :: binary().
-type metadata() :: map().
-type signature() :: [binary()].
