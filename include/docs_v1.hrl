-ifndef(DOCS_V1_H).
-define(DOCS_V1_H, true).

-record(docs_v1, {anno,
                  beam_language,
                  format,
                  module_doc,
                  metadata,
                  docs}).

-record(docs_v1_entry, {kind_name_arity,
                        anno,
                        signature,
                        doc,
                        metadata}).

-endif. %% DOCS_V1_H
