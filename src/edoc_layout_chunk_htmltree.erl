-module(edoc_layout_chunk_htmltree).

-behaviour(edoc_layout).
-export([module/2]).

-behaviour(edoc_chunks).
-export([format_xmerl/2]).

-include_lib("xmerl/include/xmerl.hrl").

-spec module(edoc:edoc_module(), list()) -> binary().
module(Doc, Options0) ->
    Options = lists:keystore(chunk_format, 1, Options0, {chunk_format, ?MODULE}),
    Chunk = edoc_chunks:edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).

-spec format_xmerl(edoc_chunks:xml_element_contents(), list()) -> any().
format_xmerl(XMLContents, Options) ->
    format_content(XMLContents, Options).

-type htmltree() :: [ht_node()].
-type ht_node() :: binary()
	         | {tag(), [attribute()], htmltree()}
	         | {tag(), htmltree()}.
-type tag() :: atom().
-type attribute() :: [{atom(), binary()}].

-spec format_content(edoc_chunks:xml_element_contents(), map()) -> htmltree().
format_content(Contents, Ctx) ->
    lists:flatten([ format_content_(C, Ctx) || C <- Contents ]).

-spec format_content_(edoc_chunks:xml_element_content(), map()) -> htmltree().
format_content_(#xmlPI{}, _Ctx)      -> [];
format_content_(#xmlComment{}, _Ctx) -> [];
format_content_(#xmlDecl{}, _Ctx)    -> [];

format_content_(#xmlAttribute{} = Attr, _Ctx) ->
    #xmlAttribute{name = Name, value = V} = Attr,
    %% From xmerl.hrl: #xmlAttribute.value :: IOlist() | atom() | integer()
    case V of
	_ when is_list(V)    -> {Name, iolist_to_binary(V)};
	_ when is_atom(V)    -> {Name, atom_to_binary(V, utf8)};
	_ when is_integer(V) -> {Name, integer_to_binary(V)}
    end;

format_content_(#xmlText{} = T, Ctx) ->
    Text = T#xmlText.value,
    iolist_to_binary(Text);

format_content_(#xmlElement{} = E, Ctx) ->
    #xmlElement{name = Name, content = Content, attributes = Attributes} = E,
    case {is_edoc_tag(Name), is_html_tag(Name)} of
	{true, _} ->
	    format_content(Content, Ctx);
	{_, false} ->
	    edoc_report:warning("'~s' is not accepted - skipping tag, extracting content", [Name]),
	    format_content(Content, Ctx);
	_ ->
	    [{Name, format_content(Attributes, Ctx), format_content(Content, Ctx)}]
    end.

-spec is_edoc_tag(atom()) -> boolean().
is_edoc_tag(fullDescription) -> true;
is_edoc_tag(_) -> false.

-spec is_html_tag(atom()) -> boolean().
is_html_tag(Tag) ->
    Tags = [a,p,h1,h2,h3,i,br,em,pre,code,ul,ol,li,dl,dt,dd],
    lists:member(Tag, Tags).
