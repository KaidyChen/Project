-module(cloudproxy_ipc_info_store).

-export([init/0]).
-export([
         insert/3,
         lookup/1,
         delete/1,
         show/0
        ]).

-define(TAB_ID, ?MODULE).

init() ->
    ?TAB_ID = ets:new(?TAB_ID, [set, public, named_table]),
    ok.

insert(IpcId, ClientPid, Ip) ->
    ets:insert(?TAB_ID, {IpcId, ClientPid, Ip}),
    ok.
                                
lookup(IpcId) ->
    case ets:lookup(?TAB_ID, IpcId) of
        [] ->
            {error, not_found};
        [{IpcId, ClientPid, Ip}] ->
            {IpcId, ClientPid, Ip}
    end.

delete(IpcId) ->
    ets:delete(?TAB_ID, IpcId),
    ok.

show() ->
    case ets:tab2list(?TAB_ID) of
        Info ->
            {ok, Info};
        [] ->
            {error, no_data}
    end.
