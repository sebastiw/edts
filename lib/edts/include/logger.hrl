
-ifndef(FUNCTION_NAME). %% OTP-19+
-define(FUNCTION_NAME, undefined).
-endif.

-ifndef(FUNCTION_ARITY). %% OTP-19+
-define(FUNCTION_ARITY, -1).
-endif.

-define(LOG_ERROR(Args), ?DO_LOG(error, "%s", Args)).
-define(LOG_ERROR(Fmt, Args), ?DO_LOG(error, Fmt, Args)).
-define(LOG_WARNING(Fmt, Args), ?DO_LOG(warning, Fmt, Args)).
-define(LOG_INFO(Fmt, Args), ?DO_LOG(info, Fmt, Args)).
-define(LOG_DEBUG(Fmt, Args), ?DO_LOG(debug, Fmt, Args)).

-define(LOCATION, #{mfa => {?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY},
                    line => ?LINE,
                    file => ?FILE}).

-define(DO_LOG(Level, Fmt, Args),
        edts_logger:log(Level, ?LOCATION, Fmt, Args)).

