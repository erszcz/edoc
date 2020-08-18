-module(eep48_SUITE_fixtures).

-export([f/0,
	 fun_with_deprecated_tag/0,
	 fun_with_since_tag/0]).

-export_type([type_with_deprecated_tag/0,
	      type_with_since_tag/0]).

%% TODO: Putting @deprecated on consecutive -type and -callback raises an error
%%	 when EDoc processes the following function.
%%	 This tag is currently not exported to chunk for types and callbacks anyway,
%%	 so it's not a critical problem, but might need addressing in the future.
%%	 FYI, a double comment "%% %%" skips EDoc tag processing - it's a "commented out comment".
%%	 See {@link edoc_tags:tags/0} for tag scope and allowed multiplicity.

%% TODO: Interestingly, the same does NOT happen for the @since tag,
%%	 which seems to be processed in the same way.

-type type_with_deprecated_tag() :: ok.
%% %% @deprecated This type is deprecated.

-type type_with_since_tag() :: ok.
%% @since 0.1.0

-callback cb_with_deprecated_tag() -> ok.
%% %% @deprecated This callback is deprecated.

-callback cb_with_since_tag() -> ok.
%% @since 0.1.0

%% @deprecated Deprecated function.
fun_with_deprecated_tag() -> ok.

%% @since 0.1.0
fun_with_since_tag() -> ok.

f() -> ok.

module_link() -> ok.

%% @doc Link to application {@link //edoc}.
app_link() -> ok.

%% @doc Link to application module {@link //edoc/edoc_doclet}.
app_module_link() -> ok.

%% @doc Link to application M:F/A {@link //edoc/edoc:files/2}.
app_mfa_link() -> ok.

%% @doc Link to external function {@link eep48_SUITE:suite/0}.
external_function_link() -> ok.

%% @doc Link to local function {@link f/0}.
local_function_link() -> ok.
