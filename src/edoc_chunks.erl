%% @doc A module to extract docs and attach them as chunks.
%% @since 0.12
-module(edoc_chunks).

-export([edoc_to_chunk/1, edoc_to_chunk/2,
         write_chunk/2]).

-export_type([docs_v1/0,
              docs_v1_entry/0,
              beam_language/0,
              mime_type/0,
              doc/0,
              doc_language/0,
              doc_string/0,
              metadata/0,
              signature/0]).

-export_type([xml_element_contents/0,
              xml_element_content/0]).

-callback format_xmerl(xml_element_content(), list()) -> any().

-include_lib("edoc/include/docs_v1.hrl").
-include_lib("xmerl/include/xmerl.hrl").

-type xml_element_contents() :: [ xml_element_content() ].
-type xml_element_content() :: #xmlElement{}
                             | #xmlText{}
                             | #xmlPI{}
                             | #xmlComment{}
                             | #xmlDecl{}
                             | #xmlAttribute{}.
%% `#xmlElement.content' as defined by `xmerl.hrl'.
%% It also contains `#xmlAttribute{}', which technically is NOT element content.

%% @type docs_v1(). The Docs v1 chunk according to EEP 48.
-type docs_v1() :: #docs_v1{anno :: erl_anno:anno(),
                            beam_language :: beam_language(),
                            format :: mime_type(),
                            module_doc :: doc(),
                            metadata :: metadata(),
                            docs :: [docs_v1_entry()]}.

-type docs_v1_entry() :: #docs_v1_entry{kind_name_arity :: {atom(), atom(), arity()},
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

%% @doc Fetch edoc docs from a given `ErlPath' and convert it to docs chunk.
%%
%% http://erlang.org/eeps/eep-0048.html
%%
%% Examples:
%%
%% ```
%% > docs_chunks:edoc_to_chunk("src/foo.erl").
%% {docs_v1, ..., erlang, <<"text/markdown">>", ..., ..., ..., ...}
%% '''
%% @end
-spec edoc_to_chunk(string()) -> docs_v1().
edoc_to_chunk(ErlPath) ->
    Includes = ["include", "src"],
    {_Module, Doc} = edoc:get_doc(ErlPath, [{preprocess, true}, {includes, Includes}]),
    edoc_to_chunk(Doc, []).

-spec edoc_to_chunk(_, _) -> docs_v1().
edoc_to_chunk(Doc, Opts) ->
    [Doc] = xmerl_xpath:string("//module", Doc),
    Metadata = edoc_extract_metadata(Doc, Opts),
    DocContents = extract_doc_contents("./description/fullDescription", Doc, Opts),
    Docs = edoc_extract_docs(Doc, Opts),
    Chunk = docs_v1(DocContents, Metadata, Docs),
    Chunk.

extract_doc_contents(XPath, Doc, Opts) ->
    case string:trim(xpath_to_text("./@private", Doc, Opts)) of
	<<"yes">> ->
	    hidden;
	<<"">> ->
	    xpath_to_chunk_format(XPath, Doc, Opts)
    end.

edoc_extract_metadata(Doc, Opts) ->
    case xpath_to_text("./since", Doc, Opts) of
        <<"">> -> #{};
        Since -> #{since => Since}
    end.

edoc_extract_docs(Doc, Opts) ->
    edoc_extract_types(Doc, Opts) ++ edoc_extract_functions(Doc, Opts).

edoc_extract_types(Doc, Opts) ->
    [edoc_extract_type(D, Opts) || D <- xmerl_xpath:string("//typedecls/typedecl", Doc)].

edoc_extract_type(Doc, Opts) ->
    Name = xpath_to_atom("./typedef/erlangName/@name", Doc, Opts),
    [#xmlElement{content=Content}] = xmerl_xpath:string("./typedef/argtypes", Doc),
    Arity = length(Content),
    DocContents = extract_doc_contents("./description/fullDescription", Doc, Opts),
    docs_v1_entry(type, Name, Arity, #{}, DocContents).

edoc_extract_functions(Doc, Opts) ->
    [edoc_extract_function(Doc1, Opts) || Doc1 <- xmerl_xpath:string("//module/functions/function", Doc)].

edoc_extract_function(Doc, Opts) ->
    Name = xpath_to_atom("./@name", Doc, Opts),
    Arity = xpath_to_integer("./@arity", Doc, Opts),
    DocContents =
        case xmerl_xpath:string("./equiv", Doc) of
            [Equiv] ->
                %% TODO: use new link syntax here
                Expr = xpath_to_text("./expr", Equiv, Opts),
                See = xpath_to_text("./see", Equiv, Opts),
                [iolist_to_binary(["Equivalent to ", "[", Expr, "](`", See, "`)."])];
            [] ->
                extract_doc_contents("./description/fullDescription", Doc, Opts)
        end,
    Metadata = edoc_extract_metadata(Doc, Opts),
    docs_v1_entry(function, Name, Arity, Metadata, DocContents).

%% @doc Append given `Chunk' to `BeamPath'.
write_chunk(BeamPath, Chunk) ->
    {ok, _Module, Chunks} = beam_lib:all_chunks(BeamPath),
    NewChunks = lists:keystore("Docs", 1, Chunks, {"Docs", term_to_binary(Chunk)}),
    {ok, Binary} = beam_lib:build_module(NewChunks),
    file:write_file(BeamPath, Binary).

%%
%% Utilities
%%

docs_v1(DocContents, Metadata, Docs) ->
    % TODO fill these in
    Anno = 0,
    BeamLanguage = erlang,
    Format = <<"application/erlang+html">>,
    {docs_v1, Anno, BeamLanguage, Format, #{<<"en">> => DocContents}, Metadata, Docs}.

docs_v1_entry(Kind, Name, Arity, Metadata, DocContents) ->
    % TODO fill these in
    Anno = 0,
    % TODO get signature from abstract code
    Signature = [list_to_binary(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))],
    {{Kind, Name, Arity}, Anno, Signature, #{<<"en">> => DocContents}, Metadata}.

xpath_to_text(XPath, Doc, Opts) ->
    string:trim(to_plain_text(xmerl_xpath:string(XPath, Doc), Opts)).

xpath_to_atom(XPath, Doc, Opts) ->
    binary_to_atom(string:trim(to_plain_text(xmerl_xpath:string(XPath, Doc), Opts)), utf8).

xpath_to_integer(XPath, Doc, Opts) ->
    binary_to_integer(string:trim(to_plain_text(xmerl_xpath:string(XPath, Doc), Opts))).

to_plain_text(Term, Opts) ->
    iolist_to_binary(edoc_layout_chunk_markdown:format_xmerl(Term, Opts)).

xpath_to_chunk_format(XPath, Doc, Opts) ->
    XMLContents = xmerl_xpath:string(XPath, Doc),
    {chunk_format, ChunkFormat} = lists:keyfind(chunk_format, 1, Opts),
    ChunkFormat:format_xmerl(XMLContents, Opts).

%%. vim: foldmethod=marker foldmarker=%%',%%.
