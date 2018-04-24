-module(cloudproxy_transmit_ipc).

-export([send/3]).

send(IpcId, HttpClientPid, Request) ->
    case cloudproxy_ipc_info_store:lookup(IpcId) of
        {_, TcpClientPid, Ip} ->
            TcpClientPid ! {middleware, HttpClientPid, Request};
        _ ->
            io:format("ipc is offline!!!~n",[])
    end.
