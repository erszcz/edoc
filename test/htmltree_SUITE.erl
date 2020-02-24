%% ``Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%%
-module(htmltree_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Test server specific exports
-export([all/0, suite/0, groups/0,
	 init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).

%% Test cases
-export([module_ref/1]).

%%
%% CT prelude
%%

suite() ->
    case os:getenv("ERL_TOP") of
        false ->
            [];
        _ ->
            [{ct_hooks,[ts_install_cth]}]
    end.

all() ->
    [module_ref].

groups() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

%%
%% Test cases
%%

module_ref(C) ->
    trace(edoc, get_doc),
    ModFile = filename:join([?config(data_dir, C), "refs.erl"]),
    Chunk = make_chunk(refs, ModFile, default_options()),
    ?debugVal(Chunk, 1000),
    ct:fail(not_done_yet).
    %lookup()

%%
%% Helpers
%%

make_chunk(Mod, File, Options) ->
    Env = edoc_lib:get_doc_env(Mod, [], Options),
    {_, Doc} = edoc:get_doc(File, Env, Options),
    Chunk = edoc:layout(Doc, Options),
    binary_to_term(Chunk).

%lookup() ->

default_options() ->
    [{layout, edoc_layout_chunk_htmltree},
     {doclet, edoc_doclet_chunks},
     {preprocess, true},
     {includes, ["/Users/erszcz/.asdf/installs/erlang/21.1/lib/kernel-6.1/include",
		 "/Users/erszcz/.asdf/installs/erlang/21.1/lib/stdlib-3.6/include/"]}].

trace(M, F) ->
    dbg:stop_clear(),
    TraceF = fun (Trace, _) ->
		     ?debugVal(Trace, 1000)
		     %ct:pal("~p~n~p~n~n", [calendar:now_to_local_time(os:timestamp()), Trace])
	     end,
    dbg:tracer(process, {TraceF, ok}),
    dbg:p(all, [call]),
    dbg:tpl(M, F, x).
