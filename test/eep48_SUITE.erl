-module(eep48_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/eep48.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2]).

%% Test cases
-export([test_metadata/1]).

%%
%% CT preamble
%%

suite() -> [].

all() -> [test_metadata].

groups() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.

%%
%% Tests
%%

test_metadata(Config) ->
    %% GIVEN
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    {ok, Chunk} = get_doc_chunk(DataDir, PrivDir, tags),
    Docs = Chunk#docs_v1.docs,
    %% WHEN / THEN
    Meta1 = get_metadata(hd(lookup_function(deprecated_example, 0, Docs))),
    ?assert(is_binary(maps:get(deprecated, Meta1))),
    Meta2 = get_metadata(hd(lookup_function(since_example, 0, Docs))),
    ?assert(is_binary(maps:get(since, Meta2))).

%%
%% Helpers
%%

get_doc_chunk(DataDir, PrivDir, Mod) ->
    TagsErl = filename:join([DataDir, atom_to_list(Mod) ++ ".erl"]),
    edoc:files([TagsErl], [{doclet, edoc_doclet_chunks},
			   {layout, edoc_layout_chunk_htmltree},
			   {dir, PrivDir}]),
    TagsChunk = filename:join([PrivDir, "chunks", atom_to_list(Mod) ++ ".chunk"]),
    {ok, BChunk} = file:read_file(TagsChunk),
    Chunk = binary_to_term(BChunk),
    {ok, Chunk}.

%% Based on shell_docs:get_doc/3.
lookup_function(Function, Arity, Docs) ->
    FnFunctions =
	lists:filter(fun({{function, F, A},_Anno,_Sig,_Doc,_Meta}) ->
			     F =:= Function andalso A =:= Arity;
			(_) ->
			     false
		     end, Docs),
    [{F, A, S, maps:get(<<"en">>, D), M} || {F,A,S,D,M} <- FnFunctions].

get_metadata({_, _, _, _, Metadata}) -> Metadata.
