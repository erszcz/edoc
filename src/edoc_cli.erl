-module(edoc_cli).
-export([main/1]).

main(Args) ->
    Opts = parse_args(Args),
    print("Running with opts:\n~p\n", [Opts]),
    ok = code:add_pathsa(maps:get(code_paths, Opts)),
    case Opts of
        #{run := app, app := App} ->
            edoc:application(App, edoc_opts(maps:get(mode, Opts)));
        #{run := files, files := Files} ->
            edoc:files(Files, edoc_opts(maps:get(mode, Opts)))
    end.

parse_args(Args) ->
    Init = #{mode => default,
	     run => app,
	     app => no_app,
	     files => [],
	     code_paths => [],
	     continue => false},
    check_opts(maps:without([continue], parse_args(Args, Init))).

parse_args([], Opts) ->
    Opts;
parse_args(["-chunks" | Args], Opts) ->
    parse_args(Args, Opts#{mode := chunks, continue := false});
parse_args(["-pa", Path | Args], Opts) ->
    #{code_paths := Paths} = Opts,
    parse_args(Args, Opts#{code_paths := Paths ++ [Path], continue := false});
parse_args(["-app", App | Args], Opts) ->
    parse_args(Args, Opts#{run := app, app := list_to_atom(App), continue := false});
parse_args(["-files" | Args], Opts) ->
    parse_args(Args, Opts#{run := files, continue := files});
parse_args([File | Args], #{continue := files} = Opts) ->
    #{files := Files} = Opts,
    parse_args(Args, Opts#{files := Files ++ [File]}).

check_opts(Opts) ->
    case Opts of
	#{run := app, app := App} when is_atom(App), App /= no_app -> ok;
	#{run := app, app := no_app} -> quit(no_app, Opts);
	#{run := files, files := [_|_]} -> ok;
	#{run := files, files := []} -> quit(no_files, Opts)
    end,
    #{mode := Mode, code_paths := CodePaths} = Opts,
    lists:member(Mode, [default, chunks]) orelse erlang:error(mode, Opts),
    is_list(CodePaths) orelse erlang:error(code_paths),
    Opts.

quit(Reason, Opts) ->
    case Reason of
	no_app ->
	    print("No app name specified\n");
	no_files ->
	    print("No files to process\n")
    end,
    print("\n"),
    print(usage()),
    erlang:halt(1).

edoc_opts(default) ->
    [{preprocess, true}];
edoc_opts(chunks) ->
    [{doclet, edoc_doclet_chunks},
     {layout, edoc_layout_chunks},
     {preprocess, true}].

print(Text) ->
    print(Text, []).

print(Fmt, Args) ->
    io:format(Fmt, Args).

usage() ->
    "Usage: edoc.escript -app App [-chunks]\n"
    "       edoc.escript -files File1 ... FileN [-chunks]\n"
    "\n"
    "Run EDoc from the command line.\n"
    "  -app   \truns edoc:application/2\n"
    "  -files \truns edoc:files/2\n"
    "  -chunks\twhen present, only doc chunks are generated\n".
