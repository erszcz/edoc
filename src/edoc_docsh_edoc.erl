-module(edoc_docsh_edoc).

-behaviour(edoc_docsh_reader).
-export([available/1,
         to_internal/1]).

%% EDoc facade
-export([format_edoc/2]).

%% Test API
-export([to_internal/2]).

-import(edoc_docsh_lib, [print/2]).

-include_lib("xmerl/include/xmerl.hrl").
-include("edoc_docsh_stacktrace.hrl").

-define(l(Args), fun () -> Args end).

-spec available(edoc_docsh_beam:t()) -> [edoc_docsh_reader:t()].
available(Beam) ->
    [ ?MODULE || edoc_docsh_lib:is_module_available(edoc),
                 edoc_docsh_lib:is_module_available(xmerl),
                 edoc_docsh_beam:source_file(Beam) /= false ].

-spec to_internal(edoc_docsh_beam:t()) -> R when
      R :: {ok, edoc_docsh_internal:t()}
         | {error, any(), [erlang:stack_item()]}.
to_internal(Beam) ->
    to_internal(Beam, []).

-spec to_internal(edoc_docsh_beam:t(), list()) -> R when
      R :: {ok, edoc_docsh_internal:t()}
         | {error, any(), [erlang:stack_item()]}.
to_internal(Beam, Opts) ->
    try
        File = case edoc_docsh_beam:source_file(Beam) of
                   false -> erlang:error(edoc_requires_source);
                   F when is_list(F) -> F
               end,
        {_Mod, EDoc} = edoc:get_doc(File, [preprocess]),
        [ write(edoc_docsh_beam:name(Beam), Tag, OutDir, dispatch(Tag, File, EDoc))
          || {OutDir, Tags} <- [proplists:get_value(debug, Opts)],
             Tag <- Tags ],
        Internal = xmerl:export_simple([EDoc], edoc_docsh_edoc_xmerl),
        {ok, Internal}
    catch ?STACKTRACE(_, R, Stacktrace)
        {error, R, Stacktrace}
    end.

-spec format_edoc(EDoc, RenderingContext) -> R
      when EDoc :: edoc_docsh_edoc_xmerl:xml_element_content(),
           RenderingContext :: any(),
           R :: iolist().
format_edoc(EDoc, Ctx) ->
    edoc_docsh_edoc_xmerl:format_edoc(EDoc, Ctx).

dispatch(source,    File, _EDoc) ->
    {ok, Content} = file:read_file(File),
    Content;
dispatch(edoc,     _File,  EDoc) -> pp(EDoc);
dispatch(xml,      _File,  EDoc) -> xmerl:export_simple([EDoc], xmerl_xml);
dispatch(html,     _File,  EDoc) -> io_lib:format("~s", [edoc:layout(EDoc)]);
dispatch(flat,     _File,  EDoc) -> pp(xmerl:export_simple([EDoc], edoc_docsh_edoc_xmerl_flat));
dispatch(internal, _File,  EDoc) -> pp(xmerl:export_simple([EDoc], edoc_docsh_edoc_xmerl));
dispatch(otpsgml,  _File,  EDoc) -> io_lib:format("~s", [xmerl:export_simple([EDoc], xmerl_otpsgml)]);
dispatch(text,     _File,  EDoc) -> pp(xmerl:export_simple([EDoc], xmerl_text)).

write(Mod, Tag, print, Content) ->
    print("\n>>> ~p.~p:\n~p\n", [Mod, Tag, Content]);
write(Mod, Tag, OutDir, Content) ->
    SMod = atom_to_list(Mod),
    STag = atom_to_list(Tag),
    file:write_file(filename:join([OutDir,  SMod ++ "." ++ STag]), Content).

pp(Content) ->
    io_lib:format("~p", [Content]).
