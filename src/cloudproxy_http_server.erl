-module(cloudproxy_http_server).

-export([start/0, dispatch/1]).
-include("config.hrl").
-include("print.hrl").

-define(TIMEOUT, 3*60*1000).

start() ->
    application:start(inets),
    [{cloud_http_port, Port}] = ets:lookup(?LISTEN_OPTS_TABLE, cloud_http_port),
    {ok, Pid} = mochiweb_http:start_link([{port, Port}, {loop, fun dispatch/1}]),
    Pid.

dispatch(Req) ->
    Raw_path = mochiweb_request:get(raw_path, Req),
    Method = mochiweb_request:get(method, Req),
    HttpClientPid = self(),
    io:format("Raw_path = ~p~n", [Raw_path]),
    [FirstPath | Rest] = string:tokens(Raw_path, "/"),
    io:format("FirstPath:~p~n",[FirstPath]),
    Response = 
            case FirstPath of
                "middleware" ->
                    Params = binary_to_list(mochiweb_request:recv_body(Req)),
                    io:format("Params:~p~n",[Params]),
                    [Head |Tail] = string:tokens(Params, "&"),
                    [_, IpcId] = string:tokens(Head, "="),
                    [_, OrderNo] = string:tokens(lists:nth(1,Tail), "="),
                    case check_sign(Params, Tail) of
                        1 ->
                          Request = "post#" ++ Raw_path ++ Params,
                          cloudproxy_transmit_ipc:send(IpcId, HttpClientPid, Request),
                          request_log(OrderNo, Params),
                          Msg = wait_reply(),
                          response_log(OrderNo, Msg),
                          Msg; 
                        0 ->
                          <<"have no permission!!!">>
                    end;
                "dataBase" ->
                    [RestPath] = Rest,
                    [_ | SecondPath] = string:tokens(RestPath, "?"),
                    case SecondPath of
                        [] ->
                            Params = binary_to_list(mochiweb_request:recv_body(Req)),
                            {ok, {_, ParseList},_} = rfc4627:decode(list_to_binary(Params)),
                            IpcId = help_handle:datafind("ipcid", ParseList),
                            cloudproxy_event_server:operation_log(IpcId, RestPath, Params),
                            Request = "head#" ++ Raw_path ++ "@" ++ Params;
                        _ ->
                            IpcId = string:right(Raw_path, 12),
                            Request = "get#" ++ Raw_path
                    end,
                    cloudproxy_transmit_ipc:send(IpcId, HttpClientPid, Request),
                    wait_reply();
                "connector" ->
                            IpcId = string:right(Raw_path, 12),
                            Request = "get#" ++ Raw_path,
                            cloudproxy_transmit_ipc:send(IpcId, HttpClientPid, Request),
                            wait_reply();
                _ ->
                    <<"Bad request!!!">>
            end,
    io:format("Response:~p~n",[Response]),
    Req:respond({200, [{"Content-Type", "application/json"}], Response}).

wait_reply() ->
    receive
        {reply, Msg} ->
            Msg
    after
        ?TIMEOUT ->
            Reason = <<"get response timeout!!!">>,
            Reason
    end.

request_log(OrderNumber, RequestContent) ->
    cloudproxy_event_server:request_log(OrderNumber, RequestContent).

response_log(OrderNumber, ResponseContent) ->
    cloudproxy_event_server:response_log(OrderNumber, ResponseContent).

check_sign(Data, List) ->
    [_, Sign]= string:tokens(lists:last(List), "="),
    RestData = string:left(Data, string:len(Data) - 39),
    Md5 = encryption:md5(RestData),
    io:format("Md5:~p~n",[Md5]),
    case Sign of
        "FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF" ->
              1;
        _ ->
              case Md5 =:= Sign of
                      true ->
                            1;  
                      false ->
                            0
              end
    end.
