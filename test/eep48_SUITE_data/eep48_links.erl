%% @doc Fixtures for chunk links.
%% See http://erlang.org/doc/apps/erl_docgen/inline_tags.html#%3Csee*%3E---see-tags for
%% the description of `seemfa', `seetype', etc link types.
-module(eep48_links).

-export([f/0,
	 module_link/0,
	 app_link/0,
	 app_module_link/0,
	 app_mfa_link/0,
	 external_function_link/0,
	 local_function_link/0,
	 local_type_link/0,
	 external_type_link/0]).

-export_type([t/0]).

-type t() :: {}.

f() -> ok.

%% @doc Link to this module {@link eep48_links}.
%% Should map to `seeerl'.
module_link() -> ok.

%% @doc Link to application {@link //edoc}.
%% Should map to `seeapp'.
app_link() -> ok.

%% @doc Link to application module {@link //edoc/edoc_doclet}.
%% Should map to `seeerl'.
app_module_link() -> ok.

%% @doc Link to application M:F/A {@link //edoc/edoc:files/2}.
%% Should map to `seemfa'.
app_mfa_link() -> ok.

%% @doc Link to external function {@link eep48_SUITE:suite/0}.
%% Should map to `seemfa'.
external_function_link() -> ok.

%% @doc Link to local function {@link f/0}.
%% Should map to `seemfa'.
local_function_link() -> ok.

%% @doc Local type link {@link t()}.
%% Should map to `seetype'.
local_type_link() -> ok.

%% @doc External type link {@link eep48_links:t()}.
%% Should map to `seetype'.
external_type_link() -> ok.
