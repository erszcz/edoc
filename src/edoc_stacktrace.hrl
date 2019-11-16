-ifndef(EDOC_STACKTRACE_H).
-define(EDOC_STACKTRACE_H, true).

-ifdef(OTP_RELEASE).
    %% For some time this clause will be implicit, since OTP_RELEASE was introduced in 21.
    -if(?OTP_RELEASE >= 21).
        -define(STACKTRACE(Type, Reason, Stacktrace), Type:Reason:Stacktrace ->).
    -endif.
-else.
    %% OTP 20 or lower.
    -define(STACKTRACE(Type, Reason, Stacktrace), Type:Reason -> Stacktrace = erlang:get_stacktrace(), ).
-endif.

-endif. %% EDOC_STACKTRACE_H
