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
	 function_since_tag/1,
	 function_deprecated_tag/1,
	 type_since_tag/1,
	 type_deprecated_tag/1,
	 cb_since_tag/1,
	 cb_deprecated_tag/1,
	 links/1]).

%%
%% CT preamble
%%

suite() -> [].

all() -> [edoc_app_should_pass_shell_docs_validation,
	  function_since_tag,
	  function_deprecated_tag,
	  type_since_tag,
	  type_deprecated_tag,
	  cb_since_tag,
	  cb_deprecated_tag,
	  links].

%% TODO: remove these cases once EDoc supports extracting the relevant tags
not_supported() -> [type_since_tag,
		    type_deprecated_tag,
		    cb_since_tag,
		    cb_deprecated_tag,
		    links].

groups() -> [].

init_per_suite(Config) -> Config.
end_per_suite(_Config) -> ok.

init_per_group(_GroupName, Config) -> Config.
end_per_group(_GroupName, _Config) -> ok.

init_per_testcase(edoc_app_should_pass_shell_docs_validation = _CaseName, Config) ->
    {ok, #{ebin := EbinDir} = CopyInfo} = copy_application(edoc, ?config(priv_dir, Config)),
    true = code:add_patha(EbinDir),
    [{edoc_copy, CopyInfo} | Config];
init_per_testcase(CaseName, Config) ->
    case lists:member(CaseName, not_supported()) of
	true ->
	    {skip, "not supported"};
	false ->
	    Config
    end.

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

function_since_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"0.1.0">>, get_function_meta_field(since, fun_with_since_tag, 0, Docs) ).

function_deprecated_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"Deprecated function.">>,
		 get_function_meta_field(deprecated, fun_with_deprecated_tag, 0, Docs) ).

type_since_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"0.1.0">>, get_type_meta_field(since, type_with_since_tag, 0, Docs) ).

type_deprecated_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"Deprecated type.">>,
		 get_type_meta_field(deprecated, type_with_deprecated_tag, 0, Docs) ).

cb_since_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"0.1.0">>,
		 get_callback_meta_field(since, cb_with_since_tag, 0, Docs) ).

cb_deprecated_tag(Config) ->
    Docs = get_docs(Config, eep48_meta),
    %?debugVal(Docs, 1000),
    ?assertEqual(<<"Deprecated callback.">>,
		 get_callback_meta_field(deprecated, cb_with_deprecated_tag, 0, Docs) ).

links(Config) ->
    Docs = get_docs(Config, eep48_links),
    ?debugVal(Docs, 1000),
    ct:fail(not_done_yet).

%%
%% Helpers
%%

get_docs(Config, M) ->
    DataDir = ?config(data_dir, Config),
    PrivDir = ?config(priv_dir, Config),
    {ok, Chunk} = get_doc_chunk(DataDir, PrivDir, M),
    Chunk#docs_v1.docs.

get_function_meta_field(Field, F, A, Docs) ->
    get_meta_field(Field, function, F, A, Docs).

get_type_meta_field(Field, T, A, Docs) ->
    get_meta_field(Field, type, T, A, Docs).

get_callback_meta_field(Field, Cb, A, Docs) ->
    get_meta_field(Field, callback, Cb, A, Docs).

get_meta_field(Field, Kind, Name, Arity, Docs) ->
    Meta = get_metadata(lookup_entry(Kind, Name, Arity, Docs)),
    maps:get(Field, Meta).

get_doc_chunk(DataDir, PrivDir, Mod) ->
    TagsErl = filename:join([DataDir, atom_to_list(Mod) ++ ".erl"]),
    edoc:files([TagsErl], [{doclet, edoc_doclet_chunks},
			   {layout, edoc_layout_chunks},
			   {dir, PrivDir}]),
    TagsChunk = filename:join([PrivDir, "chunks", atom_to_list(Mod) ++ ".chunk"]),
    {ok, BChunk} = file:read_file(TagsChunk),
    Chunk = binary_to_term(BChunk),
    {ok, Chunk}.

lookup_function(F, A, Docs) -> lookup_entry(function, F, A, Docs).

lookup_type(T, A, Docs) -> lookup_entry(type, T, A, Docs).

lookup_callback(Cb, A, Docs) -> lookup_entry(callback, Cb, A, Docs).

lookup_entry(Kind, Function, Arity, Docs) ->
    [Entry] = lists:filter(fun({{K, F, A},_Anno,_Sig,_Doc,_Meta})
				 when K =:= Kind andalso F =:= Function, A =:= Arity ->
				   true;
			      (_) ->
				   false
			   end, Docs),
    Entry.

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
    copy_app_dir(App, ebin, EbinDir),
    copy_app_dir(App, include, IncludeDir),
    copy_app_dir(App, src, SrcDir),
    {ok, #{ebin => EbinDir, doc => DocDir, src => SrcDir}}.

copy_app_dir(App, Dir, TargetDir) ->
    {ok, Files} = file:list_dir(code:lib_dir(App, Dir)),
    lists:foreach(fun (F) ->
			  file:copy(filename:join(code:lib_dir(App, Dir), F),
				    filename:join(TargetDir, F))
		  end, Files).
