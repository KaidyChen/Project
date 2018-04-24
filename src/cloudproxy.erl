%%%======================================================
%% @doc Cloudproxy API to start and stop application
%% @author KAYDI Chen
%% @datetime 2018-4-23 12:00:00
%%%======================================================

-module(cloudproxy).

-export([start/0, stop/0]).

-define(APP, ?MODULE).
-define(APPLIST, [lager, ranch, inets, ?APP]).

%% @doc Start application
start() ->
    [app_util:start_app(App) || App <- ?APPLIST],
    ok.

%% @doc Stop application
stop() ->
    [app_util:stop_app(App) || App <- ?APPLIST],
    ok.
