%%%-------------------------------------------------------------------
%% @doc pnsvc top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(pnsvc_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{
      strategy => rest_for_one,
      intensity => 1000, 
      period => 3600
     },        

    GunSup = #{
      id => {'pnsvc', 'gun_sup'},   
      start => {'gun_sup', start_link, []},      
      restart => permanent,
      shutdown => 2000, 
      type => supervisor,       
      modules => ['gun_sup']
     },

    PoolSup = #{
      id => 'pnsvc_client_pool_sup',   
      start => {'pnsvc_client_pool_sup', start_link, []},      
      restart => permanent,   
      shutdown => 2000, 
      type => supervisor,       
      modules => ['pnsvc_client_pool_sup']
     },

    WorkerNum = persistent_term:get(pnsvc_worker_num),
    WorkerSup = #{
      id => 'pnsvc_worker_sup',   
      start => {'pnsvc_worker_sup', start_link, [WorkerNum]},      
      restart => permanent,   
      shutdown => 2000, 
      type => supervisor,       
      modules => ['pnsvc_worker_sup']
     },

    Stats = #{
      id => 'pnsvc_stats',   
      start => {'pnsvc_stats', start_link, []},      
      restart => permanent,   
      shutdown => 2000, 
      type => worker, 
      modules => ['pnsvc_stats']
     },

    {ok, {SupFlags, [GunSup, PoolSup, WorkerSup, Stats]}}.
