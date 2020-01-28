-module(edoc_layout_chunk_markdown).

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

-spec format_xmerl(edoc_chunks:xml_element_contents(), list()) -> iolist().
format_xmerl(XMLContents, Options) ->
    Ctx = maps:from_list(proplists:unfold(Options)),
    lists:map(fun
		  ({br})        -> "\n";
		  ({i, Inline}) -> [Inline]
	      end, end_block(format_content(XMLContents, Ctx))).

-type internal() :: [{br} | {i, iolist()}].

-spec format_content(edoc_chunks:xml_element_contents(), map()) -> internal().
format_content(Contents, Ctx) ->
    lists:flatten([ format_content_(C, Ctx) || C <- Contents ]).

-spec format_content_(edoc_chunks:xml_element_content(), map()) -> internal().
format_content_(#xmlPI{}, _Ctx)      -> [];
format_content_(#xmlComment{}, _Ctx) -> [];
format_content_(#xmlDecl{}, _Ctx)    -> [];

format_content_(#xmlAttribute{} = Attr, _Ctx) ->
    [{i, Attr#xmlAttribute.value}];

format_content_(#xmlText{} = T, Ctx) ->
    Text = T#xmlText.value,
    case edoc_lib:is_space(Text) of
	true -> [];
	false ->
	    case is_preformatted(T#xmlText.parents) of
		true  -> cleanup_preformatted_text(Text, Ctx);
		false -> cleanup_text(Text, Ctx)
	    end
    end;

format_content_(#xmlElement{name = Name, content = Content} = E, Ctx) ->
    format_element(Name, E, format_content(Content, Ctx), Ctx).

format_element(h1, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h2, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h3, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h4, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h5, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(h6, #xmlElement{} = E, Lines, Ctx) -> format_header(E, Lines, Ctx);
format_element(hgroup, _, _Lines, _Ctx) -> [];
format_element(code, #xmlElement{}, Lines, _Ctx) ->
    [{i, "`"}, Lines, {i, "`"}];
format_element(dl, #xmlElement{}, Lines, _Ctx) ->
    end_block(Lines);
format_element(dt, #xmlElement{}, Lines, _Ctx) ->
    dl_item("  ", Lines);
format_element(dd, #xmlElement{}, Lines, _Ctx) ->
    dl_item("      ", Lines);
format_element(p, #xmlElement{}, Lines, _Ctx) ->
    end_block(lists:dropwhile(fun
				  ({br}) -> true;
				  (_) -> false
			      end, Lines));
format_element(pre, #xmlElement{}, Lines, _Ctx) ->
    end_block([{i, "```"}, {br},
	       Lines,
	       {br}, {i, "```"}]);
format_element(ol, #xmlElement{} = E, ListItems, Ctx) ->
    lists:all(fun ({li, _}) -> true; (_) -> false end, ListItems)
    orelse erlang:error({non_list_item_children, ListItems}, [ol, E, ListItems, Ctx]),
    end_block([ [{i, io_lib:format("  ~b. ", [Index])}, FirstItem, IndentedRest, {br}]
		|| {Index, {li, [FirstItem | Rest]}} <- enumerate(ListItems),
		   IndentedRest <- [prepend("    ", Rest)] ]);
format_element(ul, #xmlElement{} = E, ListItems, Ctx) ->
    lists:all(fun ({li, _}) -> true; (_) -> false end, ListItems)
    orelse erlang:error({non_list_item_children, ListItems}, [ul, E, ListItems, Ctx]),
    end_block([ [{i, "  - "}, FirstItem, IndentedRest, {br}]
		|| {li, [FirstItem | Rest]} <- ListItems,
		   IndentedRest <- [prepend("    ", Rest)] ]);
format_element(li, #xmlElement{}, Lines, _Ctx) ->
    [{li, Lines}];
format_element(_, #xmlElement{}, Lines, _Ctx) ->
    Lines.

format_header(#xmlElement{name = Name}, Lines, _Ctx) ->
    Headers = #{h1 => "# ",
		h2 => "## ",
		h3 => "### ",
		h4 => "#### ",
		h5 => "##### ",
		h6 => "###### "},
    case Name of
	hgroup -> [];
	_ ->
	    [{i, Text}] = Lines,
	    end_block([{br}, {i, [maps:get(Name, Headers), Text]}])
    end.

cleanup_text(Text, _Ctx) ->
    lists:flatmap(fun
		      ("\n") -> [{br}];
		      (T) ->
			  case edoc_lib:is_space(T) of
			      true -> [];
			      false -> [{i, T}]
			  end
		  end,
		  split(Text, "\s*(\n)\s*", [trim, {return, list}])).

cleanup_preformatted_text(Text, _Ctx) ->
    lists:flatmap(fun
		      ("\n") -> [{br}];
		      (T) -> [{i, T}]
		  end,
		  split(Text, "(\n)", [{return, list}])).

split(Text, Pattern, Opts) ->
    re:split(Text, Pattern, Opts).

is_preformatted(Parents) ->
    lists:any(fun
		  ({pre, _}) -> true;
		  (_) -> false
	      end, Parents).

prepend(Prefix, Doc) -> prepend(Prefix, lists:reverse(Doc), []).

prepend(_Prefix, [], Acc) -> Acc;
prepend( Prefix, [{br} | Doc], [] = Acc) -> prepend(Prefix, Doc, [{br} | Acc]);
prepend( Prefix, [{br} | Doc], [{br}] = Acc) -> prepend(Prefix, Doc, [{br} | Acc]);
prepend( Prefix, [{br} | Doc], [{br}, {br}] = Acc) -> prepend(Prefix, Doc, Acc);
prepend( Prefix, [{br} | Doc], Acc) -> prepend(Prefix, Doc, [{br}, {i, Prefix} | Acc]);
prepend( Prefix, [Node | Doc], Acc) -> prepend(Prefix, Doc, [Node | Acc]).

enumerate(List) ->
    lists:zip(lists:seq(1, length(List)), List).

-spec end_block(_) -> internal().
end_block(Doc) -> end_block([{br}, {br} | lists:reverse(lists:flatten(Doc))], []).

end_block([], Acc) -> Acc;
end_block([{br} | Doc], [] = Acc) -> end_block(Doc, [{br} | Acc]);
end_block([{br} | Doc], [{br}] = Acc) -> end_block(Doc, [{br} | Acc]);
end_block([{br} | Doc], [{br}, {br}] = Acc) -> end_block(Doc, Acc);
end_block([Node | Doc], Acc) -> end_block(Doc, [Node | Acc]).

dl_item(Prefix, Lines) ->
    [First | Rest] = Lines,
    end_block([{i, Prefix}, First, prepend(Prefix, Rest)]).
