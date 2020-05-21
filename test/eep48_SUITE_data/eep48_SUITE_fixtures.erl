-module(eep48_SUITE_fixtures).

-export([deprecated_example/0,
	 since_example/0]).

%% @deprecated See {@link file/2} for details.
deprecated_example() -> ok.

%% @since 0.1.0
since_example() -> ok.
