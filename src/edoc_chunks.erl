%% @doc A module to extract docs and attach them as chunks.
%% @since 0.12
-module(edoc_chunks).

-export([edoc_to_chunk/1,
         otp_xml_to_chunk/2,
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

-include_lib("edoc/include/docs_v1.hrl").
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
    [Doc] = xmerl_xpath:string("//module", Doc),
    Metadata = edoc_extract_metadata(Doc),
    DocContents = extract_doc_contents("./description/fullDescription", Doc),
    Docs = edoc_extract_docs(Doc),
    Chunk = docs_v1(DocContents, Metadata, Docs),
    Chunk.

extract_doc_contents(XPath, Doc) ->
    case xpath_to_binary("./@private", Doc) of
        <<"yes">> ->
            hidden;
        <<"">> ->
            case xpath_to_binary(XPath, Doc) of
                <<"">> ->
                    none;
                Binary ->
                    #{<<"en">> => Binary}
            end
    end.

edoc_extract_metadata(Doc) ->
    case xpath_to_binary("./since", Doc) of
        <<"">> -> #{};
        Since -> #{since => Since}
    end.

edoc_extract_docs(Doc) ->
    edoc_extract_types(Doc) ++ edoc_extract_functions(Doc).

edoc_extract_types(Doc) ->
    [edoc_extract_type(D) || D <- xmerl_xpath:string("//typedecls/typedecl", Doc)].

edoc_extract_type(Doc) ->
    Name = xpath_to_atom("./typedef/erlangName/@name", Doc),
    [#xmlElement{content=Content}] = xmerl_xpath:string("./typedef/argtypes", Doc),
    Arity = length(Content),
    DocContents = extract_doc_contents("./description/fullDescription", Doc),
    docs_v1_entry(type, Name, Arity, #{}, DocContents).

edoc_extract_functions(Doc) ->
    [edoc_extract_function(Doc1) || Doc1 <- xmerl_xpath:string("//module/functions/function", Doc)].

edoc_extract_function(Doc) ->
    Name = xpath_to_atom("./@name", Doc),
    Arity = xpath_to_integer("./@arity", Doc),
    DocContents =
        case xmerl_xpath:string("./equiv", Doc) of
            [Equiv] ->
                Expr = xpath_to_binary("./expr", Equiv),
                See = xpath_to_binary("./see", Equiv),
                Binary = iolist_to_binary(["Equivalent to ", "[", Expr, "](`", See, "`)."]),
                #{<<"en">> => Binary};
            [] ->
                extract_doc_contents("./description/fullDescription", Doc)
        end,
    Metadata = edoc_extract_metadata(Doc),
    docs_v1_entry(function, Name, Arity, Metadata, DocContents).

%% @doc Extract XML docs from `XMLPath' in `OTPRootDir' and convert it to docs chunk.
otp_xml_to_chunk(OTPRootDir, XMLPath) ->
    Doc = load_otp_xml(OTPRootDir, filename:join([OTPRootDir, XMLPath])),
    DocContents = extract_doc_contents("//description", Doc),
    Docs = otp_xml_extract_docs(Doc, XMLPath),
    docs_v1(DocContents, #{}, Docs).

%% TODO: extract types and callbacks too
otp_xml_extract_docs(Doc, XMLPath) ->
    List = [otp_xml_extract_function(Doc1, XMLPath) || Doc1 <- xmerl_xpath:string("./funcs/func", Doc)],
    lists:filter(fun(Elem) -> Elem /= skip end, List).

otp_xml_extract_function(Doc, XMLPath) ->
    % 'Elixir.IO':inspect(xmerl_xpath:string("./name/@name", Doc)),
    Name = xpath_to_atom("./name/@name", Doc),

    case Name == '' of
        true ->
            warn_unparsable(Doc, XMLPath),
            skip;
        false ->
            Arity = xpath_to_integer("./name/@arity", Doc),
            DocContents = extract_doc_contents("./desc", Doc),
            % case Name of
            %     take ->
            %         'Elixir.IO':inspect(xmerl_xpath:string("//desc", Doc)),
            %         'Elixir.IO':inspect(DocString),
            %         ok;
            %     _ ->
            %         ok
            % end,
            docs_v1_entry(function, Name, Arity, #{}, DocContents)
    end.

% TODO: example warning:
%
%   gen_server.xml: cannot parse call(ServerRef, Request) -> Reply
%
% so pretty important function! Need to gracefully handle this.
warn_unparsable(Doc, XMLPath) ->
    Head = to_markdown(xmerl_xpath:string("//name[1]/text()", Doc)),
    io:fwrite("~s: cannot parse ~s~n", [XMLPath, Head]).


load_otp_xml(OTPRootDir, XMLPath) ->
    Options = [
               % {space, normalize},
               {fetch_path, [OTPRootDir ++ "/lib/erl_docgen/priv/dtd",
                             OTPRootDir ++ "/lib/erl_docgen/priv/dtd_html_entities" ]}],
    XMLPath2 = filename:join([OTPRootDir, XMLPath]),
    {Doc, []} = xmerl_scan:file(XMLPath2, Options),
    Doc.

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
    Format = <<"text/markdown">>,
    {docs_v1, Anno, BeamLanguage, Format, DocContents, Metadata, Docs}.

docs_v1_entry(Kind, Name, Arity, Metadata, DocContents) ->
    % TODO fill these in
    Anno = 0,
    % TODO get signature from abstract code
    Signature = [list_to_binary(atom_to_list(Name) ++ "/" ++ integer_to_list(Arity))],
    {{Kind, Name, Arity}, Anno, Signature, DocContents, Metadata}.

xpath_to_binary(XPath, Doc) ->
    to_markdown(xmerl_xpath:string(XPath, Doc)).

xpath_to_atom(XPath, Doc) ->
    binary_to_atom(to_markdown(xmerl_xpath:string(XPath, Doc)), utf8).

xpath_to_integer(XPath, Doc) ->
    binary_to_integer(to_markdown(xmerl_xpath:string(XPath, Doc))).

to_markdown(Term) ->
    iolist_to_binary(format_edoc(Term)).

%% @type xml_element_content(). `#xmlElement.content' as defined by `xmerl.hrl'.
-type xml_element_content() :: [#xmlElement{} | #xmlText{} | #xmlPI{} | #xmlComment{} | #xmlDecl{}].

-spec format_edoc(xml_element_content()) -> iolist().
format_edoc(Content) ->
    Ctx = #{},
    lists:map(fun
                  ({br})        -> "\n";
                  ({i, Inline}) -> [Inline]
              end, end_block(format_content(Content, Ctx))).

format_content(Content, Ctx) ->
    lists:flatten([ format_content_(C, Ctx) || C <- Content ]).

format_content_(#xmlPI{}, _Ctx)      -> [];
format_content_(#xmlComment{}, _Ctx) -> [];
format_content_(#xmlDecl{}, _Ctx)    -> [];

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
    Lines;
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
    end_block(Lines);
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

end_block(Doc) -> end_block([{br}, {br} | lists:reverse(lists:flatten(Doc))], []).

end_block([], Acc) -> Acc;
end_block([{br} | Doc], [] = Acc) -> end_block(Doc, [{br} | Acc]);
end_block([{br} | Doc], [{br}] = Acc) -> end_block(Doc, [{br} | Acc]);
end_block([{br} | Doc], [{br}, {br}] = Acc) -> end_block(Doc, Acc);
end_block([Node | Doc], Acc) -> end_block(Doc, [Node | Acc]).

dl_item(Prefix, Lines) ->
    [First | Rest] = Lines,
    end_block([{i, Prefix}, First, prepend(Prefix, Rest)]).

%%. vim: foldmethod=marker foldmarker=%%',%%.
