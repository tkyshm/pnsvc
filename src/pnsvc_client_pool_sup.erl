%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-01-15 17:21:31.489343
%%%-------------------------------------------------------------------
-module(pnsvc_client_pool_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(DEFAULT_POOL_SIZE, 2).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    SupFlags = #{
      strategy => one_for_one,
      intensity => 1000, 
      period => 3600
     },        

    PoolSize = application:get_env(pnsvc, pool_size, ?DEFAULT_POOL_SIZE),
    AChild = #{
      id => 'pnsvc_client_pool',   
      start => {'pnsvc_client_pool', start_link, [PoolSize]},
      restart => permanent,
      shutdown => 2000, 
      type => worker,       
      modules => ['pnsvc_client_pool']
     },

    {ok, {SupFlags, [AChild]}}.
