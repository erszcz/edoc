%% =====================================================================
%% Licensed under the Apache License, Version 2.0 (the "License"); you may
%% not use this file except in compliance with the License. You may obtain
%% a copy of the License at <http://www.apache.org/licenses/LICENSE-2.0>
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% Alternatively, you may use this file under the terms of the GNU Lesser
%% General Public License (the "LGPL") as published by the Free Software
%% Foundation; either version 2.1, or (at your option) any later version.
%% If you wish to allow use of your version of this file only under the
%% terms of the LGPL, you should delete the provisions above and replace
%% them with the notice and other provisions required by the LGPL; see
%% <http://www.gnu.org/licenses/>. If you do not delete the provisions
%% above, a recipient may use your version of this file under the terms of
%% either the Apache License or the LGPL.
%%
%% @copyright 2019 Radek Szymczyszyn
%% @author Radek Szymczyszyn <lavrin@gmail.com>
%% @end
%% =====================================================================

%% @doc Doclet generating standalone EEP-48 Docs chunk files.
%% @reference See http://erlang.org/eeps/eep-0048.html.
%% @end

%% Note that this is written so that it is *not* depending on edoc.hrl!

-module(edoc_doclet_chunks).

-export([run/2]).

-import(edoc_report, [report/2, warning/2]).

%% @headerfile "../include/edoc_doclet.hrl"
-include("../include/edoc_doclet.hrl").

%-define(EDOC_APP, edoc).
-define(DEFAULT_FILE_SUFFIX, ".chunk").
-define(CHUNKS_SUBDIR, "chunks").
%-define(INDEX_FILE, "index.html").
%-define(OVERVIEW_FILE, "overview.edoc").
%-define(OVERVIEW_SUMMARY, "overview-summary.html").
%-define(MODULES_FRAME, "modules-frame.html").
%-define(STYLESHEET, "stylesheet.css").
%-define(IMAGE, "erlang.png").
%-define(NL, "\n").

-include_lib("xmerl/include/xmerl.hrl").

-spec run(edoc_doclet:command(), edoc_doclet:context()) -> ok.
run(#doclet_gen{} = Cmd, Ctxt) ->
    gen(Cmd#doclet_gen.sources,
	Cmd#doclet_gen.app,
	Cmd#doclet_gen.modules,
	Ctxt);
run(#doclet_toc{} = Cmd, Ctxt) ->
    erlang:error(not_implemented).
    %toc(Cmd#doclet_toc.paths, Ctxt).

gen(Sources, App, Modules, Ctxt) ->
    Dir = filename:join(Ctxt#doclet_context.dir, ?CHUNKS_SUBDIR),
    Env = Ctxt#doclet_context.env,
    Options = Ctxt#doclet_context.opts,
    {Modules1, Error} = sources(Sources, Dir, Modules, Env, Options),
    ok.


%% NEW-OPTIONS: title
%% DEFER-OPTIONS: run/2

title(App, Options) ->
    proplists:get_value(title, Options,
			if App == ?NO_APP ->
				"Overview";
			   true ->
				io_lib:fwrite("Application: ~ts", [App])
			end).


%% @doc Process the individual source files.
%%
%% NEW-OPTIONS: file_suffix, private, hidden
%% INHERIT-OPTIONS: edoc:layout/2
%% INHERIT-OPTIONS: edoc:get_doc/3
%% DEFER-OPTIONS: run/2
sources(Sources, Dir, Modules, Env, Options) ->
    Suffix = proplists:get_value(file_suffix, Options, ?DEFAULT_FILE_SUFFIX),
    Private = proplists:get_bool(private, Options),
    Hidden = proplists:get_bool(hidden, Options),
    {Ms, E} = lists:foldl(fun (Src, {Set, Error}) ->
				  source(Src, Dir, Suffix, Env, Set,
					 Private, Hidden, Error, Options)
			  end,
			  {sets:new(), false}, Sources),
    {[M || M <- Modules, sets:is_element(M, Ms)], E}.


%% @doc Write a chunk file for a source file.
%%
%% Add its name to the set if it was successful.
%% Errors are just flagged at this stage,
%% allowing all source files to be processed even if some of them fail.
source({M, Name, Path}, Dir, Suffix, Env, OkSet, Private, Hidden, ErrorFlag, Options) ->
    File = filename:join(Path, Name),
    try
	Chunk = edoc_chunks:edoc_to_chunk(File),
	WriteOptions = [{encoding, utf8}],
	ok = write_file(term_to_binary(Chunk), Dir, chunk_file_name(Name, Suffix), WriteOptions),
	{sets:add_element(Name, OkSet), ErrorFlag}
    catch _:R:St ->
	report("skipping source file '~ts': ~tP.", [File, R, 15]),
	io:format("stacktrace:\n~p\n", [St]),
	{OkSet, true}
    end.

chunk_file_name(ErlName, Suffix) ->
    string:join([filename:basename(ErlName, ".erl"), Suffix], "").

write_file(Data, Dir, Name, Options) ->
    File = filename:join([Dir, Name]),
    ok = filelib:ensure_dir(File),
    case file:write_file(File, Data) of
	ok -> ok;
	{error, R} ->
	    R1 = file:format_error(R),
	    report("could not write file '~ts': ~ts.", [File, R1]),
	    exit(error)
    end.

check_name(M, M0, File) ->
    N = M,
    N0 = M0,
    case N of
	[$? | _] ->
	    %% A module name of the form '?...' is assumed to be caused
	    %% by the epp_dodger parser when the module declaration has
	    %% the form '-module(?MACRO).'; skip the filename check.
	    ok;
	_ ->
	    if N =/= N0 ->
		    warning("file '~ts' actually contains module '~s'.",
			    [File, M]);
	       true ->
		    ok
	    end
    end,
	ok.

%% Creating an index file, with some frames optional.
%% TODO: get rid of frames, or change doctype to Frameset

%index_file(Dir, Title) ->
%    Frame2 = {frame, [{src,?MODULES_FRAME},
%                      {name,"modulesFrame"},{title,""}],
%              []},
%    Frame3 = {frame, [{src,?OVERVIEW_SUMMARY},
%                      {name,"overviewFrame"},{title,""}],
%              []},
%    Frameset = {frameset, [{cols,"20%,80%"}],
%            [?NL, Frame2, ?NL, ?NL, Frame3, ?NL,
%                    {noframes,
%                     [?NL,
%                      {h2, ["This page uses frames"]},
%                      ?NL,
%                      {p, ["Your browser does not accept frames.",
%                           ?NL, br,
%                           "You should go to the ",
%                           {a, [{href, ?OVERVIEW_SUMMARY}],
%                            ["non-frame version"]},
%                           " instead.", ?NL]},
%                      ?NL]},
%                    ?NL]},
%    XML = xhtml_1(Title, [], Frameset),
%    Text = xmerl:export_simple([XML], xmerl_html, []),
%    edoc_lib:write_file(Text, Dir, ?INDEX_FILE).

%modules_frame(Dir, Ms, Title, CSS) ->
%    Body = [?NL,
%            {h2, [{class, "indextitle"}], ["Modules"]},
%            ?NL,
%            {table, [{width, "100%"}, {border, 0},
%                     {summary, "list of modules"}],
%             lists:append(
%               [[?NL,
%                 {tr, [{td, [],
%                        [{a, [{href, module_ref(M)},
%                              {target, "overviewFrame"},
%                              {class, "module"}],
%                          [atom_to_list(M)]}]}]}]
%                 || M <- Ms])},
%            ?NL],
%    XML = xhtml(Title, CSS, Body),
%    Text = xmerl:export_simple([XML], xmerl_html, []),
%    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

%module_ref(M) ->
%    atom_to_list(M) ++ ?DEFAULT_FILE_SUFFIX.

%xhtml(Title, CSS, Content) ->
%    xhtml_1(Title, CSS, {body, [{bgcolor, "white"}], Content}).

%xhtml_1(Title, CSS, Body) ->
%    {html, [?NL,
%            {head, [?NL, {title, [Title]}, ?NL] ++ CSS},
%            ?NL,
%            Body,
%            ?NL]
%    }.

%% NEW-OPTIONS: overview
%% INHERIT-OPTIONS: read_file/4
%% INHERIT-OPTIONS: edoc_lib:run_layout/2
%% INHERIT-OPTIONS: edoc_extract:file/4
%% DEFER-OPTIONS: run/2

%overview(Dir, Title, Env, Opts) ->
%    File = proplists:get_value(overview, Opts,
%                               filename:join(Dir, ?OVERVIEW_FILE)),
%    Encoding = edoc_lib:read_encoding(File, [{in_comment_only, false}]),
%    Tags = read_file(File, overview, Env, Opts),
%    Data0 = edoc_data:overview(Title, Tags, Env, Opts),
%    EncodingAttribute = #xmlAttribute{name = encoding,
%                                      value = atom_to_list(Encoding)},
%    #xmlElement{attributes = As} = Data0,
%    Data = Data0#xmlElement{attributes = [EncodingAttribute | As]},
%    F = fun (M) ->
%                M:overview(Data, Opts)
%        end,
%    Text = edoc_lib:run_layout(F, Opts),
%    EncOpts = [{encoding,Encoding}],
%    edoc_lib:write_file(Text, Dir, ?OVERVIEW_SUMMARY, EncOpts).

%copy_image(Dir) ->
%    case code:priv_dir(?EDOC_APP) of
%        PrivDir when is_list(PrivDir) ->
%            From = filename:join(PrivDir, ?IMAGE),
%            edoc_lib:copy_file(From, filename:join(Dir, ?IMAGE));
%        _ ->
%            report("cannot find default image file.", []),
%            exit(error)
%    end.

%% NEW-OPTIONS: stylesheet_file
%% DEFER-OPTIONS: run/2

%copy_stylesheet(Dir, Options) ->
%    case proplists:get_value(stylesheet, Options) of
%        undefined ->
%            From = case proplists:get_value(stylesheet_file, Options) of
%                       File when is_list(File) ->
%                           File;
%                       _ ->
%                           case code:priv_dir(?EDOC_APP) of
%                               PrivDir when is_list(PrivDir) ->
%                                   filename:join(PrivDir, ?STYLESHEET);
%                               _ ->
%                                   report("cannot find default "
%                                          "stylesheet file.", []),
%                                   exit(error)
%                           end
%                   end,
%            edoc_lib:copy_file(From, filename:join(Dir, ?STYLESHEET));
%        _ ->
%            ok
%    end.

%% NEW-OPTIONS: stylesheet
%% DEFER-OPTIONS: run/2

%stylesheet(Options) ->
%    case proplists:get_value(stylesheet, Options) of
%        "" ->
%            [];
%        S ->
%            Ref = case S of
%                      undefined ->
%                          ?STYLESHEET;
%                      "" ->
%                          "";    % no stylesheet
%                      S when is_list(S) ->
%                          S;
%                      _ ->
%                          report("bad value for option 'stylesheet'.",
%                                 []),
%                          exit(error)
%                  end,
%            [{link, [{rel, "stylesheet"},
%                     {type, "text/css"},
%                     {href, Ref},
%                     {title, "EDoc"}], []},
%             ?NL]
%    end.

is_private(E) ->
    case get_attrval(private, E) of
 	"yes" -> true;
 	_ -> false
    end.

is_hidden(E) ->
    case get_attrval(hidden, E) of
 	"yes" -> true;
 	_ -> false
    end.

encoding(E) ->
    case get_attrval(encoding, E) of
        "latin1" -> latin1;
        _ -> utf8
    end.

get_attrval(Name, #xmlElement{attributes = As}) ->
    case get_attr(Name, As) of
	[#xmlAttribute{value = V}] ->
	    V;
	[] -> ""
    end.

get_attr(Name, [#xmlAttribute{name = Name} = A | As]) ->
    [A | get_attr(Name, As)];
get_attr(Name, [_ | As]) ->
    get_attr(Name, As);
get_attr(_, []) ->
    [].

%% Read external source file. Fails quietly, returning empty tag list.

%% INHERIT-OPTIONS: edoc_extract:file/4

read_file(File, Context, Env, Opts) ->
    case edoc_extract:file(File, Context, Env, Opts) of
	{ok, Tags} ->
	    Tags;
	{error, _} ->
	    []
    end.


%% TODO: FIXME: meta-level index generation

%% Creates a Table of Content from a list of Paths (ie paths to applications)
%% and an overview file.

-define(EDOC_DIR, "doc").
-define(INDEX_DIR, "doc/index").
-define(CURRENT_DIR, ".").

%toc(Paths, Ctxt) ->
%    Opts = Ctxt#doclet_context.opts,
%    Dir = Ctxt#doclet_context.dir,
%    Env = Ctxt#doclet_context.env,
%    app_index_file(Paths, Dir, Env, Opts).

%% TODO: FIXME: it's unclear how much of this is working at all

%% NEW-OPTIONS: title
%% INHERIT-OPTIONS: overview/4

%app_index_file(Paths, Dir, Env, Options) ->
%    Title = proplists:get_value(title, Options,"Overview"),
%%    Priv = proplists:get_bool(private, Options),
%    CSS = stylesheet(Options),
%    Apps1 = [{filename:dirname(A),filename:basename(A)} || A <- Paths],
%    index_file(Dir, Title),
%    application_frame(Dir, Apps1, Title, CSS),
%    modules_frame(Dir, [], Title, CSS),
%    overview(Dir, Title, Env, Options),
%%    edoc_lib:write_info_file(Prod, [], Modules1, Dir),
%    copy_stylesheet(Dir, Options).

%application_frame(Dir, Apps, Title, CSS) ->
%    Body = [?NL,
%            {h2, ["Applications"]},
%            ?NL,
%            {table, [{width, "100%"}, {border, 0}],
%             lists:append(
%               [[{tr, [{td, [], [{a, [{href,app_ref(Path,App)},
%                                      {target,"_top"}],
%                                  [App]}]}]}]
%                || {Path,App} <- Apps])},
%            ?NL],
%    XML = xhtml(Title, CSS, Body),
%    Text = xmerl:export_simple([XML], xmerl_html, []),
%    edoc_lib:write_file(Text, Dir, ?MODULES_FRAME).

%app_ref(Path,M) ->
%    filename:join([Path,M,?EDOC_DIR,?INDEX_FILE]).
