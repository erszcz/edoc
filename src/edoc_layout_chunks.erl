%% @doc Convert EDoc module documentation to an EEP-48 `docs_v1' chunk.
%% @since 0.12
-module(edoc_layout_chunks).

-behaviour(edoc_layout).
-export([module/2]).

-include("edoc.hrl").

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

-type docs_v1() :: #docs_v1{anno :: erl_anno:anno(),
                            beam_language :: beam_language(),
                            format :: mime_type(),
                            module_doc :: doc(),
                            metadata :: metadata(),
                            docs :: [docs_v1_entry()]}.
%% The Docs v1 chunk according to EEP 48.

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

-type xmerl_document_node() :: #xmlElement{}
                             | #xmlText{}
                             | #xmlPI{}
                             | #xmlComment{}
                             | #xmlDecl{}.
%% `#xmlElement.content' as defined by `xmerl.hrl'.

-type xmerl_attribute() :: #xmlAttribute{}.

-type xpath() :: string().

%%
%%' EDoc layout callbacks
%%

%% @doc Convert EDoc module documentation to an EEP-48 style doc chunk.
-spec module(edoc:xmerl_module(), proplists:proplist()) -> binary().
module(Doc, Options) ->
    %% Require `entries' or fail.
    case lists:keyfind(entries, 1, Options) of
	{entries, _} -> ok;
	_ -> erlang:error(no_entries, [Doc, Options])
    end,
    Chunk = edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).

%%.
%%' Chunk construction
%%

-spec edoc_to_chunk(edoc:xmerl_module(), proplists:proplist()) -> docs_v1().
edoc_to_chunk(Doc, Opts) ->
    [Doc] = xmerl_xpath:string("//module", Doc),
    Anno = anno(Doc, Opts),
    ModuleDoc = doc_contents("./description/fullDescription", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts)),
    Docs = doc_entries(Doc, Opts),
    docs_v1(Anno, ModuleDoc, Metadata, Docs).

-spec doc_contents(XPath, Doc, Opts) -> doc() when
      XPath :: xpath(),
      Doc :: edoc:xmerl_module(),
      Opts :: proplists:proplist().
doc_contents(XPath, Doc, Opts) ->
    case {xpath_to_text("./@private", Doc, Opts),
	  xpath_to_text("./@hidden", Doc, Opts)}
    of
	{<<"yes">>, _} ->
	    %% EDoc `@private' is EEP-48 `hidden'
	    hidden;
	{_, <<"yes">>} ->
	    %% EDoc `@hidden' is EEP-48 `none'
	    none;
	_ ->
	    doc_content(xpath_to_chunk(XPath, Doc), Opts)
    end.


meta_deprecated(Doc, Opts) ->
    Deprecated = xpath_to_text("./deprecated/description/fullDescription", Doc, Opts),
    [{deprecated, Deprecated} || is_truthy(Deprecated)].

meta_since(Doc, Opts) ->
    Since = xpath_to_text("./since", Doc, Opts),
    [{since, Since} || is_truthy(Since)].

is_truthy(<<>>) -> false;
is_truthy(B) when is_binary(B) -> true.

doc_entries(Doc, Opts) ->
    types(Doc, Opts) ++ callbacks(Doc, Opts) ++ functions(Doc, Opts).

types(Doc, Opts) ->
    [type(TD, Opts) || TD <- xmerl_xpath:string("//typedecls/typedecl", Doc)].

type(Doc, Opts) ->
    Name = xpath_to_atom("./typedef/erlangName/@name", Doc, Opts),
    [#xmlElement{content=Content}] = xmerl_xpath:string("./typedef/argtypes", Doc),
    Arity = length(Content),
    Anno = anno(Doc, Opts),
    EntryDoc = doc_contents("./description/fullDescription", Doc, Opts),
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts) ++
			      meta_type_sig(Name, Anno, entries(Opts))),
    docs_v1_entry(type, Name, Arity, Anno, EntryDoc, Metadata).

-spec meta_type_sig(atom(), erl_anno:anno(), [edoc:entry()]) -> Metadata when
      Metadata :: #{signature => erl_parse:abstract_form()}.
meta_type_sig(Name, Anno, Entries) ->
    Line = erl_anno:line(Anno),
    Tags = edoc_data:get_all_tags(Entries),
    case lists:keyfind(Line, #tag.line, Tags) of
	#tag{name = type, line = Line, origin = code} = T ->
	    TypeTree = T#tag.form,
	    TypeAttr = erl_syntax:revert(TypeTree),
	    %% Assert that the lookup by line really gives us the right type attribute:
	    {attribute, Line, type, {Name, _, _}} = TypeAttr,
	    [{signature, [TypeAttr]}];
	_ ->
	    []
    end.

callbacks(Doc, Opts) ->
    [callback(C, Opts) || C <- xmerl_xpath:string("//module/callbacks/callback", Doc)].

callback(Doc, Opts) ->
    Name = xpath_to_atom("./@name", Doc, Opts),
    Arity = xpath_to_integer("./@arity", Doc, Opts),
    %% TODO: callback annotations - edoc_data does not get this info from source, but from
    %% running `M:behaviour_info/1', so there's more work needed to get `-callback' line info.
    %% `edoc_specs' seems like the place to extract this info and pass on as an Edoc `#tag{}'.
    Anno = erl_anno:new(0),
    EntryDoc = none,
    Metadata = #{},
    docs_v1_entry(callback, Name, Arity, Anno, EntryDoc, Metadata).

functions(Doc, Opts) ->
    [function(F, Opts) || F <- xmerl_xpath:string("//module/functions/function", Doc)].

function(Doc, Opts) ->
    Name = xpath_to_atom("./@name", Doc, Opts),
    Arity = xpath_to_integer("./@arity", Doc, Opts),
    Anno = anno(Doc, Opts),
    EntryDoc =
	case xmerl_xpath:string("./equiv", Doc) of
	    [Equiv] ->
		%% TODO: use new link syntax here
		Expr = xpath_to_text("./expr", Equiv, Opts),
		See = xpath_to_text("./see", Equiv, Opts),
		Content = [iolist_to_binary(["Equivalent to ", "[", Expr, "](`", See, "`)."])],
		doc_content(Content, Opts);
	    [] ->
		doc_contents("./description/fullDescription", Doc, Opts)
	end,
    Metadata = maps:from_list(meta_deprecated(Doc, Opts) ++
			      meta_since(Doc, Opts) ++
			      meta_function_sig({Name, Arity}, entries(Opts))),
    docs_v1_entry(function, Name, Arity, Anno, EntryDoc, Metadata).

-spec meta_function_sig(edoc:function_name(), [edoc:entry()]) -> Metadata when
      Metadata :: #{signature => erl_parse:abstract_form()}.
meta_function_sig(NA, Entries) ->
    #entry{name = NA} = E = lists:keyfind(NA, #entry.name, Entries),
    case lists:keyfind(spec, #tag.name, E#entry.data) of
	false -> [];
	#tag{name = spec} = T ->
	    [{signature, [erl_syntax:revert(T#tag.form)]}]
    end.

-spec entries(proplists:proplist()) -> [edoc:entry()].
entries(Opts) ->
    {entries, Entries} = lists:keyfind(entries, 1, Opts),
    Entries.

-spec doc_content(_, _) -> doc().
doc_content([], _Opts) -> none;
doc_content(Content, Opts) ->
    DocLanguage = proplists:get_value(lang, Opts, <<"en">>),
    #{DocLanguage => Content}.

docs_v1(Anno, ModuleDoc, Metadata, Docs) ->
    #docs_v1{anno = Anno,
             module_doc = ModuleDoc,
             metadata = Metadata,
             docs = Docs}.

anno(Doc, Opts) ->
    {source, File} = lists:keyfind(source, 1, Opts),
    Line = xpath_to_integer("./@line", Doc, Opts),
    erl_anno:set_file(File, erl_anno:new(Line)).

-spec docs_v1_entry(_, _, _, _, _, _) -> docs_v1_entry().
docs_v1_entry(Kind, Name, Arity, Anno, EntryDoc, Metadata) ->
    % TODO get signature from abstract code
    Signature = [list_to_binary(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))],
    {{Kind, Name, Arity}, Anno, Signature, EntryDoc, Metadata}.

-spec xpath_to_text(_, _, _) -> binary().
xpath_to_text(XPath, Doc, Opts) ->
    case xmerl_xpath:string(XPath, Doc) of
	[] -> <<>>;
	[#xmlAttribute{} = Attr] ->
	    {_ , Value} = format_attribute(Attr),
	    hd(shell_docs:normalize([Value]));
	[#xmlElement{}] = Elements ->
	    iolist_to_binary(chunk_to_text(xmerl_to_chunk(Elements)));
	[_|_] ->
	    erlang:error(multiple_nodes, [XPath, Doc, Opts])
    end.

chunk_to_text([]) -> [];
chunk_to_text([Node | Nodes]) ->
    case Node of
	_ when is_binary(Node) -> [Node | chunk_to_text(Nodes)];
	{_Tag, _Attrs, SubNodes} -> [chunk_to_text(SubNodes) | chunk_to_text(Nodes)]
    end.

xpath_to_atom(XPath, Doc, Opts) ->
    binary_to_atom(xpath_to_text(XPath, Doc, Opts), utf8).

xpath_to_integer(XPath, Doc, Opts) ->
    binary_to_integer(xpath_to_text(XPath, Doc, Opts)).

xpath_to_chunk(XPath, Doc) ->
    XmerlDoc = xmerl_xpath:string(XPath, Doc),
    xmerl_to_chunk(XmerlDoc).

%%.
%%' Xmerl to chunk format
%%

%% TODO: shell_docs:chunk_elements() is not exported yet.
-spec xmerl_to_chunk(edoc:xmerl_module()) -> shell_docs:chunk_elements().
xmerl_to_chunk(Contents) ->
    shell_docs:normalize(format_content(Contents)).

-spec format_content(edoc:xmerl_module()) -> shell_docs:chunk_elements().
format_content(Contents) ->
    lists:flatten([ format_content_(C) || C <- Contents ]).

-spec format_content_(xmerl_document_node()) -> shell_docs:chunk_elements().
format_content_(#xmlPI{})      -> [];
format_content_(#xmlComment{}) -> [];
format_content_(#xmlDecl{})    -> [];

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
	    [{Name, format_attributes(Attributes), format_content(Content)}]
    end.

-spec format_attributes([xmerl_attribute()]) -> [shell_docs:chunk_element_attr()].
format_attributes(Attrs) ->
    [ format_attribute(Attr) || Attr <- Attrs ].

-spec format_attribute(xmerl_attribute()) -> shell_docs:chunk_element_attr().
format_attribute(#xmlAttribute{} = Attr) ->
    #xmlAttribute{name = Name, value = V} = Attr,
    %% From xmerl.hrl: #xmlAttribute.value :: IOlist() | atom() | integer()
    case V of
	_ when is_list(V)    -> {Name, unicode:characters_to_binary(V)};
	_ when is_atom(V)    -> {Name, atom_to_binary(V, utf8)};
	_ when is_integer(V) -> {Name, integer_to_binary(V)}
    end.

-spec is_edoc_tag(atom()) -> boolean().
is_edoc_tag(fullDescription) -> true;
is_edoc_tag(since) -> true;
is_edoc_tag(_) -> false.

-spec is_html_tag(atom()) -> boolean().
is_html_tag(Tag) ->
    %% This is only a subset of existing HTML tags.
    %% Compare with https://developer.mozilla.org/en-US/docs/Web/HTML/Element
    Tags = [a,p,h1,h2,h3,i,br,em,pre,code,ul,ol,li,dl,dt,dd],
    lists:member(Tag, Tags).

%%. vim: foldmethod=marker foldmarker=%%',%%.
