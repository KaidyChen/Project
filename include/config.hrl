%%Config info

-define(HELP, help).

%%Listen options config filepath
-define(LISTEN_OPTS_TABLE, cloudproxy_listen_opts).
-define(LISTEN_OPTS_FILEPATH, "rel/net.config").

%%Log record options
-define(LOG_DIR, "logs").
%%Ipc log dir
-define(IPC_LOG_DIR, "login").
%%Request and response log dir
-define(REQUEST_RESPONSE_LOG_DIR, "request_response").
%%Operation log dir
-define(OPERATION_LOG_DIR, "operation").
%%Log file extension
-define(LOG_FILE_EXTENSION, ".log").

%%Login msg type
-define(LOGIN_MSG_TYPE, "login").
%%Request msg type
-define(REQUEST_MSG_TYPE, "request").
%%Response msg type
-define(RESPONSE_MSG_TYPE, "response").
%%Operation msg type
-define(OPERATION_MSG_TYPE, "operation").
