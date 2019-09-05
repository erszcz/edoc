-module(edoc_docsh_writer).

-callback from_internal(edoc_docsh_internal:t()) -> any().
