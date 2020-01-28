-module(edoc_layout_chunk_htmltree).

-behaviour(edoc_layout).
-export([module/2]).

-behaviour(edoc_chunk_format).
-export([format/1]).

module(Doc, Options0) ->
    Options = lists:keystore(chunk_format, 1, Options0, {chunk_format, ?MODULE}),
    Chunk = edoc_chunks:edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).

%-spec format(XMLDoc :: xml_element_content()) -> any().
format(XMLDoc) ->
    %% TODO: write the new impl
    erlang:error(not_implemented).
