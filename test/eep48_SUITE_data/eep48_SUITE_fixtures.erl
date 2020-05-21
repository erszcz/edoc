-module(eep48_SUITE_fixtures).

-export([deprecated_f/0,
	 since_f/0]).

-export_type([deprecated_t/0,
	      since_t/0]).

%% TODO: Putting @deprecated on consecutive -type and -callback raises an error
%%	 when EDoc processes the following function.
%%	 This tag is currently not exported to chunk for types and callbacks anyway,
%%	 so it's not a critical problem, but might need addressing in the future.
%%	 FYI, a double comment "%% %%" skips EDoc tag processing - it's a "commented out comment".
%%	 See {@link edoc_tags:tags/0} for tag scope and allowed multiplicity.

%% TODO: Interestingly, the same does NOT happen for the @since tag,
%%	 which seems to be processed in the same way.

-type deprecated_t() :: ok.
%% %% @deprecated This type is deprecated.

-type since_t() :: ok.
%% @since 0.1.0

-callback deprecated_cb() -> ok.
%% %% @deprecated This callback is deprecated.

-callback since_cb() -> ok.
%% @since 0.1.0

%% @deprecated See {@link file/2} for details.
deprecated_f() -> ok.

%% @since 0.1.0
since_f() -> ok.
