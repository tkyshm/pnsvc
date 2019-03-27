%%%-------------------------------------------------------------------
%% @doc pnsvc public API
%% @end
%%%-------------------------------------------------------------------
-module(pnsvc_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(DEFAULT_WORKER_NUM, 5).
-define(DEFAULT_EXPIRE, 3600).

-ifdef(TEST).
-define(DEFAULT_SERVICE_ACCOUNT, [code:lib_dir(pnsvc, priv), "/test_service_account.json"]).
-else.
-define(DEFAULT_SERVICE_ACCOUNT, [code:lib_dir(pnsvc, priv), "/service_account.json"]).
-endif.

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    set_service_account(),
    set_worker_num(),
    set_access_token_expires(),

    Ret = pnsvc_sup:start_link(),

    Dispatch = cowboy_router:compile([
        {'_', [{"/v1/projects/:project_id/send", pnsvc_v1_send_handler, []}]}
    ]),
    Port = application:get_env(pnsvc, api_port, 5000),
    Args = [{port, Port}],
    Opts = #{env => #{dispatch => Dispatch}},
    cowboy:start_clear(pnsvc_api_server, Args, Opts),      

    Ret.


%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
set_service_account() ->
    Path = application:get_env(pnsvc, service_account_file, ?DEFAULT_SERVICE_ACCOUNT),
    {ok, Binary} = file:read_file(Path),
    Data = jsone:decode(Binary),
    persistent_term:put(pnsvc_service_account, Data).

set_worker_num() ->
    WorkerNum = application:get_env(pnsvc, worker_num, ?DEFAULT_WORKER_NUM),
    persistent_term:put(pnsvc_worker_num, WorkerNum).

set_access_token_expires() ->
    Expire = application:get_env(pnsvc, access_token_expires, ?DEFAULT_EXPIRE),
    persistent_term:put(pnsvc_access_token_expires, Expire).
