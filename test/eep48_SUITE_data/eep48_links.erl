-module(eep48_links).

-export([module_link/0,
	 app_link/0,
	 app_module_link/0,
	 app_mfa_link/0,
	 external_function_link/0,
	 local_function_link/0]).

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
