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
-module(edoc_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Test server specific exports
-export([all/0, suite/0,groups/0,init_per_suite/1, end_per_suite/1,
	 init_per_group/2,end_per_group/2]).

%% Test cases
-export([app/1,
	 build_std/1,
	 build_map_module/1,
	 otp_12008/1,
	 build_app/1,
	 otp_14285/1]).

suite() ->
    case os:getenv("ERL_TOP") of
        false ->
            [];
        _ ->
            [{ct_hooks,[ts_install_cth]}]
    end.

all() ->
    [app,
     %% TODO: reenable, possibly with https://github.com/lrascao/rebar3_appup_plugin
     %appup,
     build_std,
     build_map_module,
     otp_12008,
     build_app,
     otp_14285].

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

%% Test that the .app file does not contain any `basic' errors
app(Config) when is_list(Config) ->
    ok = ?t:app_test(edoc).

%% Test that the .appup file does not contain any `basic' errors
appup(Config) when is_list(Config) ->
    ok = ?t:appup_test(edoc).

build_std(suite) -> [];
build_std(doc) -> ["Build some documentation using standard EDoc layout"];
build_std(Config) when is_list(Config) ->
    DataDir = ?config(data_dir, Config),
    Overview1 = filename:join(DataDir, "overview.edoc"),
    Overview2 = filename:join(DataDir, "overview.syntax_tools"),
    PrivDir = ?config(priv_dir, Config),

    ok = edoc:application(edoc, [{overview, Overview1},
	    {def, {vsn,"TEST"}},
	    {dir, PrivDir}]),

    ok = edoc:application(syntax_tools, [{overview, Overview2},
	    {def, {vsn,"TEST"}},
	    {dir, PrivDir}]),

    ok = edoc:application(xmerl, [{preprocess,true},{dir, PrivDir}]),
    ok.

build_map_module(Config) when is_list(Config) ->
    DataDir  = ?config(data_dir, Config),
    PrivDir  = ?config(priv_dir, Config),
    Filename = filename:join(DataDir, "map_module.erl"),
    ok = edoc:file(Filename, [{dir, PrivDir}]),
    ok.

otp_12008(Config) when is_list(Config) ->
    DataDir  = ?config(data_dir, Config),
    PrivDir  = ?config(priv_dir, Config),
    %% rebar3 tries to compile all files under test/, but un3.erl won't compile of course
    Un1In = filename:join(DataDir, "un1.erl.in"),
    Un2In = filename:join(DataDir, "un2.erl.in"),
    Un3In = filename:join(DataDir, "un3.erl.in"),
    Un1 = filename:join(PrivDir, "un1.erl"),
    Un2 = filename:join(PrivDir, "un2.erl"),
    Un3 = filename:join(PrivDir, "un3.erl"),
    [ {ok, _} = file:copy(From, To)
      || {From, To} <- [{Un1In, Un1}, {Un2In, Un2}, {Un3In, Un3}] ],
    %% epp_dodger
    Opts1 = [{dir, PrivDir}],
    ok = edoc:files([Un1], Opts1),
    ok = edoc:files([Un2], Opts1),
    {'EXIT', error} = (catch edoc:files([Un3], Opts1)),
    %% epp
    Opts2 = [{preprocess, true}, {dir, PrivDir}],
    ok = edoc:files([Un1], Opts2),
    ok = edoc:files([Un2], Opts2),
    {'EXIT', error} = (catch edoc:files([Un3], Opts2)),
    ok.

build_app(suite) -> [];
build_app(doc) -> ["Build a local app with nested source directories"];
build_app(Config) ->
    DataDir  = ?config(data_dir, Config),
    PrivDir  = ?config(priv_dir, Config),
	OutDir = filename:join(PrivDir, "myapp"),
	Src = filename:join(DataDir, "myapp"),

	ok = edoc:application(myapp, Src, [{dir, OutDir}, {subpackages, false}]),
	true = filelib:is_regular(filename:join(OutDir, "a.html")),
	false = filelib:is_regular(filename:join(OutDir, "b.html")),

	ok = edoc:application(myapp, Src, [{dir, OutDir}]),
	true = filelib:is_regular(filename:join(OutDir, "a.html")),
	true = filelib:is_regular(filename:join(OutDir, "b.html")),
	ok.

otp_14285(Config) ->
    DataDir  = ?config(data_dir, Config),
    PrivDir  = ?config(priv_dir, Config),
    Un1 = filename:join(DataDir, "un_atom1.erl"),
    Un2 = filename:join(DataDir, "un_atom2.erl"),
    %% epp_dodger
    Opts1 = [{dir, PrivDir}],
    ok = edoc:files([Un1], Opts1),
    ok = edoc:files([Un2], Opts1),
    %% epp
    Opts2 = [{preprocess, true}, {dir, PrivDir}],
    ok = edoc:files([Un1], Opts2),
    ok = edoc:files([Un2], Opts2),
    ok.
