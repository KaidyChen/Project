-module(cloudproxy_channel_protocol).
-behaviour(gen_server).

-include("config.hrl").
-include("print.hrl").

-export([start_link/4]).

-export([
         init/1,
         init/4,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {
            listenerPid :: ranch:ref(),
            selfPid :: pid(),
            socket :: inet:socket(),
            transport :: module(),
            clientSendRecvPid = undefined :: pid() | undefined,
            lastPacket = <<>> :: binary()
            }).

start_link(ListenerPid, Socket, Transport, Opts) ->
    proc_lib:start_link(?MODULE, init, [ListenerPid, Socket, Transport, Opts]).

init([]) ->
    {ok, undefined}.

init(ListenerPid, Socket, Transport, _Opts) ->
    SelfPid = self(),
    ok = proc_lib:init_ack({ok, SelfPid}),
    ok = ranch:accept_ack(ListenerPid),
    ok = Transport:setopts(Socket, [{active, once}]),
    %%erlang:process_flag(trap_exit, true),
    State = #state{
                listenerPid = ListenerPid,
                selfPid = SelfPid,
                socket = Socket,
                transport = Transport,
                clientSendRecvPid = undefined,
                lastPacket = <<>>
                },
    gen_server:enter_loop(?MODULE, [], State, 0).

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

terminate(Reason, State) ->
    #state{
       selfPid = SelfPid,
       socket = Socket,
       transport = Transport
       } = State,
    ?PRINT("~p terminate~n",[SelfPid]),
    ?PRINT("Reason:~p, State:~p~n",[Reason, State]),
    Transport:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({tcp_closed, _Socket}, State) ->
    ?PRINT("tcp_closed!!!~n",[]),
    {stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    ?PRINT("tcp_closed!!!~n",[]),
    {stop, Reason, State};
handle_info(timeout, State) ->
    ?PRINT("timeout!!!~n",[]),
    {stop, normal, State};
handle_info({tcp, Socket, Packet = <<16#FA,16#07,16#13,16#02,16#FA,16#01,IpcIdBinary:6/binary-unit:8,16#FA,16#FF>>}, State) ->
    #state{
        selfPid = SelfPid,
        transport = Transport,
        socket = Socket
        } = State,
    ok = Transport:setopts(Socket, [{active, once}]),
    IpcId = hex_util:to_hex(IpcIdBinary),
    LoginPacket = hex_util:to_hex(Packet),
    {ok, {Ip, Port}} = Transport:peername(Socket),
    case inet:ntoa(Ip) of
        {error, _} ->
            io:format("Address info error~n",[]);
        Address ->
            cloudproxy_ipc_info_store:insert(IpcId, SelfPid, Ip),
            Msg = string:join([Address, LoginPacket], " "),
            io:format("Address:~s~n",[Msg]),
            cloudproxy_event_server:ipc_login_log(IpcId, Msg)
    end,
    {noreply, State};
handle_info({tcp, Socket, Packet}, State) ->
    #state{
        selfPid = SelfPid,
        transport = Transport,
        socket = Socket,
        clientSendRecvPid = ClientPid,
        lastPacket = LastPacket
        } = State,
    io:format("recvDataBin:~s~n",[Packet]),
    ok = Transport:setopts(Socket, [{active, once}]),
    DataStr = binary_to_list(Packet),
    Length = string:len(DataStr),
    io:format("Length:~p~n",[Length]),
    case Length >= 1460 of
        true ->
            %%LastPacket = LastPacket ++ Packet,
            NewLastPacket = binary:list_to_bin([LastPacket, Packet]),
            NewState = State#state{lastPacket = NewLastPacket},
            {noreply, NewState};
        false ->
            NewLastPacket = binary:list_to_bin([LastPacket, Packet]),
            NewState = State#state{lastPacket = <<>>},
            ClientPid ! {reply, NewLastPacket},
            {noreply, NewState}
    end;
handle_info({middleware, ClientPid, Request}, State) ->
    #state{
        transport = Transport,
        socket = Socket
        } = State,
    ok = Transport:setopts(Socket, [{active, once}]),
    RequestBin = list_to_binary(Request),
    ok = Transport:send(Socket, RequestBin),
    NewState = State#state{clientSendRecvPid = ClientPid},
    {noreply, NewState};
handle_info(_Info, State) ->
    #state{
        transport = Transport,
        socket = Socket
        } = State,
    ok = Transport:setopts(Socket, [{active, once}]),
    io:format("receive unknown data!!!~n",[]),
    {noreply, State}.
