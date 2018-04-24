-module(cloudproxy_event_server).

-behaivor(gen_event).

%% API
-export([
         start_link/0,
         add_handler/2,
         delete_handler/2
]).
-export([
         ipc_login_log/2,
         operation_log/3,
         request_log/2,
         response_log/2
]).

-define(SERVER, ?MODULE).

start_link() ->
    {ok, Pid} = gen_event:start_link({local, ?SERVER}),
    %% Add event handler
    add_handler(),
    {ok, Pid}.

%% Add event handler
add_handler() ->
    cloudproxy_event_ipc_log:add_handler(),
    cloudproxy_event_request_response_log:add_handler(),
    cloudproxy_event_operation_log:add_handler(),
    ok.   

add_handler(Handler, Args) ->
    gen_event:add_handler(?SERVER, Handler, Args).

delete_handler(Handler, Args) ->
    gen_event:delete_handler(?SERVER, Handler, Args).


ipc_login_log(IpcId, Msg) ->
    gen_event:notify(?SERVER, {ipc_login_log, IpcId, Msg}).

request_log(OrderNumber, Msg) ->
    gen_event:notify(?SERVER, {request_log, OrderNumber, Msg}).

response_log(OrderNumber, Msg) ->
    gen_event:notify(?SERVER, {response_log, OrderNumber, Msg}).

operation_log(IpcId, Operation, Msg) ->
    gen_event:notify(?SERVER, {operation_log, IpcId, Operation, Msg}).

