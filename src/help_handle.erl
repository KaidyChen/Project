-module(help_handle).

-compile(export_all).
-include("config.hrl").
-include("print.hrl").

connect_url(Path, DataList, Message) ->
    io:format("Path:~p Datalist:~p Msg:~p~n",[Path, DataList, Message]),
    Url = Path ++ DataList,
    application:start(inets),
    io:format("Url:~p  Message:~p~n",[Url, Message]),
    httpc:request(post, {Url, [], "application/x-www-form-urlencoded", Message}, [{timeout, 300000}, {connect_timeout, 300000}],[]).

datafind(DataList, ParseList) ->
    case lists:keyfind(DataList, 1, ParseList) of
        {_, DataListTmp} ->
            case is_binary(DataListTmp) of
                true ->
                    binary_to_list(DataListTmp);
                false ->
                    DataListTmp
            end;
        _ ->
            ""
    end.

parse_list(Data) ->
    {ok, Obj_Json, []} = rfc4627:decode(Data),
    {_, ParseList} = Obj_Json,
    ParseList.

get_url(IpcId) ->
    [{ipc_http_port, IpcHttpPort}] = ets:lookup(?LISTEN_OPTS_TABLE, ipc_http_port),
    case cloudproxy_ipc_info_store:lookup(IpcId) of
           {_, _, Ip} ->
              Url = "http://" ++ Ip ++ ":" ++ integer_to_list(IpcHttpPort),
              Url;
           _  ->
              ?ERROR("ipc is not online!!!",[])
    end.
