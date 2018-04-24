-module(cloudproxy_app).

-behaviour(application).

-include("config.hrl").
-include("print.hrl").

%% Application callbacks
-export([
         start/2,
         stop/1
        ]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    {ok, Sup} = cloudproxy_sup:start_link(),
    init(),
    load_config(),
    start_servers(Sup),
    {ok, Sup}.

stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init() ->
    ?LISTEN_OPTS_TABLE = ets:new(?LISTEN_OPTS_TABLE, [set, public, named_table]),
    cloudproxy_ipc_info_store:init(),
    ok.

load_config() ->
    {ok, Terms} = file:consult(?LISTEN_OPTS_FILEPATH),
    ets:insert(?LISTEN_OPTS_TABLE, Terms),
    ok.

start_servers(Sup) ->
    Servers = [
               {"Cloudproxy tcp server", fun start_tcp/0},
               {"Cloudproxy http server", fun start_http/0},
               {"Cloudproxy event server", cloudproxy_event_server}
              ],
    [start_server(Sup, Server) || Server <- Servers].

start_server(_Sup, {Name, Fun}) when is_function(Fun) ->
    ?INFO("~s is starting...", [Name]),
    Fun(),
    ?INFO_MSG("[done]");
start_server(Sup, {Name, Server}) ->
    ?INFO("~s is starting...", [Name]),
    app_util:start_child(Sup, Server),
    ?INFO_MSG("[done]");
start_server(Sup, {Name, Server, Opts}) ->
    ?INFO("~s is starting...", [Name]),
    app_util:start_child(Sup, Server, Opts),
    ?INFO_MSG("[done]");
start_server(_, _) ->
    erlang:error(badarg).

start_tcp() ->
    [{cloud_tcp_port, CloudTcpPort}] = ets:lookup(?LISTEN_OPTS_TABLE, cloud_tcp_port),
    {ok, _} = ranch:start_listener(cloudproxy_channel, 100,
                                    ranch_tcp, [{port, CloudTcpPort}, {max_connections, 20480}],
                                    cloudproxy_channel_protocol, []),
    ?INFO("CloudTcpPort: ~p", [CloudTcpPort]),
    ok.

start_http() ->
    cloudproxy_http_server:start().
