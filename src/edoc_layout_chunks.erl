%% @doc Convert EDoc module documentation to an EEP-48 `docs_v1' chunk.
%% @since 0.12
-module(edoc_layout_chunks).

-behaviour(edoc_layout).
-export([module/2]).

-export_type([docs_v1/0,
              docs_v1_entry/0,
              beam_language/0,
              mime_type/0,
              doc/0,
              doc_language/0,
              doc_string/0,
              metadata/0,
              signature/0]).

-include_lib("kernel/include/eep48.hrl").
-include_lib("xmerl/include/xmerl.hrl").

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

-type xmerl_document() :: [ xmerl_document_node() ].
-type xmerl_document_node() :: #xmlElement{}
                             | #xmlText{}
                             | #xmlPI{}
                             | #xmlComment{}
                             | #xmlDecl{}
                             | #xmlAttribute{}.
%% `#xmlElement.content' as defined by `xmerl.hrl'.
%% It also contains `#xmlAttribute{}', which technically is NOT element content.

%%
%%' EDoc layout callbacks
%%

%% @doc Convert EDoc module documentation to an EEP-48 style doc chunk.
-spec module(edoc:edoc_module(), list()) -> binary().
module(Doc, Options) ->
    Chunk = edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).

%%.
%%' Chunk construction
%%

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
	    xpath_to_chunk(XPath, Doc)
    end.

edoc_extract_metadata(Doc, Opts) ->
    Since = xpath_to_text("./since", Doc, Opts),
    Deprecated = xpath_to_text("./deprecated/description/fullDescription", Doc, Opts),
    maps:from_list([{since, Since} || truthy(Since) ] ++
		   [{deprecated, Deprecated} || truthy(Deprecated) ]).

truthy(<<>>) -> false;
truthy(B) when is_binary(B) -> true.

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

%%.
%%' Utilities
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
    iolist_to_binary(chunk_to_text(xmerl_to_chunk(Term))).

chunk_to_text([]) -> [];
chunk_to_text([Node | Nodes]) ->
    case Node of
	_ when is_binary(Node) -> [Node | chunk_to_text(Nodes)];
	{_AttrName, Value} -> [Value | chunk_to_text(Nodes)];
	{_Tag, _Attrs, SubNodes} -> [chunk_to_text(SubNodes) | chunk_to_text(Nodes)]
    end.

xpath_to_chunk(XPath, Doc) ->
    XmerlDoc = xmerl_xpath:string(XPath, Doc),
    xmerl_to_chunk(XmerlDoc).

%%.
%%' Xmerl to chunk format
%%

%% TODO: shell_docs:chunk_elements() is not exported yet.
-spec xmerl_to_chunk(xmerl_document()) -> shell_docs:chunk_elements().
xmerl_to_chunk(Contents) ->
    format_content(Contents).

-spec format_content(xmerl_document()) -> shell_docs:chunk_elements().
format_content(Contents) ->
    lists:flatten([ format_content_(C) || C <- Contents ]).

-spec format_content_(xmerl_document_node()) -> shell_docs:chunk_elements().
format_content_(#xmlPI{})      -> [];
format_content_(#xmlComment{}) -> [];
format_content_(#xmlDecl{})    -> [];

format_content_(#xmlAttribute{} = Attr) ->
    #xmlAttribute{name = Name, value = V} = Attr,
    %% From xmerl.hrl: #xmlAttribute.value :: IOlist() | atom() | integer()
    case V of
	_ when is_list(V)    -> {Name, unicode:characters_to_binary(V)};
	_ when is_atom(V)    -> {Name, atom_to_binary(V, utf8)};
	_ when is_integer(V) -> {Name, integer_to_binary(V)}
    end;

format_content_(#xmlText{} = T) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
	true -> [];
	false -> [unicode:characters_to_binary(Text)]
    end;

format_content_(#xmlElement{} = E) ->
    #xmlElement{name = Name, content = Content, attributes = Attributes} = E,
    case {is_edoc_tag(Name), is_html_tag(Name)} of
	{true, _} ->
	    format_content(Content);
	{_, false} ->
	    edoc_report:warning("'~s' is not accepted - skipping tag, extracting content", [Name]),
	    format_content(Content);
	_ ->
	    [{Name, format_content(Attributes), format_content(Content)}]
    end.

-spec is_edoc_tag(atom()) -> boolean().
is_edoc_tag(fullDescription) -> true;
is_edoc_tag(since) -> true;
is_edoc_tag(_) -> false.

-spec is_html_tag(atom()) -> boolean().
is_html_tag(Tag) ->
    Tags = [a,p,h1,h2,h3,i,br,em,pre,code,ul,ol,li,dl,dt,dd],
    lists:member(Tag, Tags).

%%. vim: foldmethod=marker foldmarker=%%',%%.
