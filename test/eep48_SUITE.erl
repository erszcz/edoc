-module(eep48_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/eep48.hrl").

%% Test server callbacks
-export([suite/0, all/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2, end_per_group/2,
	 init_per_testcase/2, end_per_testcase/2]).

%% Test cases
-export([edoc_app_should_pass_shell_docs_validation/1,
	 test_metadata/1]).

%%
%% CT preamble
%%

suite() -> [].

all() -> [edoc_app_should_pass_shell_docs_validation,
	  test_metadata].

groups() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.

init_per_testcase(edoc_app_should_pass_shell_docs_validation = _CaseName, Config) ->
    {ok, #{ebin := EbinDir} = CopyInfo} = copy_application(edoc, ?config(priv_dir, Config)),
    true = code:add_patha(EbinDir),
    [{edoc_copy, CopyInfo} | Config];
init_per_testcase(_CaseName, Config) -> Config.

end_per_testcase(edoc_app_should_pass_shell_docs_validation = _CaseName, Config) ->
    #{ebin := EbinDir} = ?config(edoc_copy, Config),
    true = code:del_path(EbinDir),
    Config;
end_per_testcase(_CaseName, Config) -> Config.

%%
%% Tests
%%

edoc_app_should_pass_shell_docs_validation(Config) ->
    ok = edoc:application(edoc, [{doclet, edoc_doclet_chunks},
				 {layout, edoc_layout_chunks},
				 private, hidden]),
    ok = application:load(edoc),
    {ok, Modules} = application:get_key(edoc, modules),
    [ shell_docs:validate(M) || M <- Modules ].

test_metadata(Config) ->
    %% GIVEN
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    {ok, Chunk} = get_doc_chunk(DataDir, PrivDir, eep48_SUITE_fixtures),
    Docs = Chunk#docs_v1.docs,
    %?debugVal(Docs, 1000),
    %% WHEN / THEN
    Meta1 = get_metadata(hd(lookup_function(since_f, 0, Docs))),
    ?assert(is_binary(maps:get(since, Meta1))),
    Meta2 = get_metadata(hd(lookup_function(deprecated_f, 0, Docs))),
    ?assert(is_binary(maps:get(deprecated, Meta2))).

%%
%% Helpers
%%

get_doc_chunk(DataDir, PrivDir, Mod) ->
    TagsErl = filename:join([DataDir, atom_to_list(Mod) ++ ".erl"]),
    edoc:files([TagsErl], [{doclet, edoc_doclet_chunks},
			   {layout, edoc_layout_chunks},
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
    [{F, A, S, D, M} || {F,A,S,D,M} <- FnFunctions].

get_metadata({_, _, _, _, Metadata}) -> Metadata.

copy_application(App, undefined) ->
    ct:fail("~s: target dir undefined", [?FUNCTION_NAME]);
copy_application(App, TargetDir) ->
    DocDir	= filename:join([TargetDir, App, "doc"]),
    EbinDir	= filename:join([TargetDir, App, "ebin"]),
    IncludeDir	= filename:join([TargetDir, App, "include"]),
    SrcDir	= filename:join([TargetDir, App, "src"]),
    ok = file:make_dir(filename:join([TargetDir, App])),
    ok = file:make_dir(DocDir),
    ok = file:make_dir(EbinDir),
    ok = file:make_dir(IncludeDir),
    ok = file:make_dir(SrcDir),
    copy_app_dir(ebin, EbinDir),
    copy_app_dir(include, IncludeDir),
    copy_app_dir(src, SrcDir),
    {ok, #{ebin => EbinDir, doc => DocDir, src => SrcDir}}.

copy_app_dir(Dir, TargetDir) ->
    {ok, Files} = file:list_dir(code:lib_dir(App, Dir)),
    lists:foreach(fun (F) ->
			  file:copy(filename:join(code:lib_dir(App, Dir), F),
				    filename:join(TargetDir, F))
		  end, Files).
