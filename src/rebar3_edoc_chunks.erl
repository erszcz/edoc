%% @doc Store modules' documentation in Docs chunks according to EEP-48.
%%
%% Add the following code to your project's `rebar.config':
%%
%% ```
%% {plugins,
%%  [
%%   %% Avoid name clash with OTP EDoc until the fork is merged.
%%   {rebar3_edoc_chunks, {git, "https://github.com/erszcz/edoc.git"}}
%%  ]}.
%%
%% {provider_hooks,
%%  [
%%   {post, [{compile, {edoc_chunks, compile}}]}
%%  ]}.
%% '''
%%
%% @end
-module(rebar3_edoc_chunks).

-behaviour(provider).
-export([init/1,
         do/1,
         format_error/1]).

-define(PROVIDER, compile).
-define(DEPS, [{default, compile}]).
-define(SHORT_DESC, "Store modules' documentation in Docs chunks according to EEP-48").
-define(DESC, "Store modules' documentation in Docs chunks according to EEP-48.\n"
              "This exposes Erlang module documentation to Elixir and other BEAM languages.\n").

%%
%% Public API
%%

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    POpts = [
             {name, ?PROVIDER},             % The 'user friendly' name of the task
             {namespace, edoc_chunks},
             {module, ?MODULE},             % The module implementation of the task
             {bare, true},                  % The task can be run by the user, always true
             {deps, ?DEPS},                 % The list of dependencies
             {opts, []},                    % list of options understood by the plugin
             {short_desc, ?SHORT_DESC},
             {desc, ?DESC}
            ],
    Provider = providers:create(POpts),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Apps = case rebar_state:current_app(State) of
               undefined ->
                   rebar_state:project_apps(State);
               AppInfo ->
                   [AppInfo]
           end,
    [ process_app(State, App) || App <- Apps ],
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io:format("error: ~p\n", [Reason]).

%%
%% Helpers
%%

-spec process_app(rebar_state:t(), rebar_app_info:t()) -> ok.
process_app(State, App) ->
    Dir = rebar_app_info:dir(App),
    EbinDir = rebar_app_info:ebin_dir(App),
    %% TODO: this will not work for non-flat src/ hierarchies
    Wildcard = filename:join([Dir, "src", "*.erl"]),
    Files = filelib:wildcard(Wildcard),
    [ process_file(State, EbinDir, F) || F <- Files ],
    ok.

-spec process_file(rebar_state:t(), file:filename(), file:filename()) -> ok.
process_file(_State, EbinDir, ErlPath) ->
    Basename = filename:basename(ErlPath, ".erl"),
    BeamPath = filename:join([EbinDir, Basename ++ ".beam"]),
    Docs = edoc_chunks:edoc_to_chunk(ErlPath),
    {ok, NewBeam} = store_chunk(BeamPath, "Docs", Docs),
    ok = file:write_file(BeamPath, NewBeam).

store_chunk(BeamPath, ChunkName, Chunk) ->
    {ok, _, AllChunks} = beam_lib:all_chunks(BeamPath),
    ChunkEntry = {ChunkName, term_to_binary(Chunk, [compressed])},
    NewChunks = lists:keystore(ChunkName, 1, AllChunks, ChunkEntry),
    {ok, _NewBEAM} = beam_lib:build_module(NewChunks).
