%%=====================================================================
%%Print msg macro
%%=====================================================================

-define(ERROR_MSG(F), lager:error(F)).
-define(WARNING(F), lager:warning(F)).
-define(INFO_MSG(F), ok).

-define(ERROR(F, D), lager:error(F, D)).
-define(WARNING(F, D), lager:warning(F, D)).
-define(INFO(F, D), ok).

-ifdef(debug).
-define(DEBUG(Msg), io:format(Msg)).
-define(DEBUG(Format, Args), io:format(Format, Args)).
-define(PRINT_MSG(Msg), io:format(Msg)).
-define(PRINT(Format, Args), io:format(Format, Args)).

-else.

-define(DEBUG(Msg), ok).
-define(DEBUG(Format, Args), ok).
-define(PRINT_MSG(Msg), ok).
-define(PRINT(Format, Args), ok).

-endif.

%%======================================================================
%%End print msg macro
%%======================================================================
