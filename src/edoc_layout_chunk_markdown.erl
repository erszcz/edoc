-module(edoc_layout_chunk_markdown).

-behaviour(edoc_layout).
-export([module/2]).

module(Doc, Options) ->
    Chunk = edoc_chunks:edoc_to_chunk(Doc, Options),
    term_to_binary(Chunk).
