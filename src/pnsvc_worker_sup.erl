%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-01-15 17:21:09.784347
%%%-------------------------------------------------------------------
-module(pnsvc_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
start_link(WorkerNum) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [WorkerNum]).

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
init([WorkerNum]) ->
    SupFlags = #{
      strategy => one_for_one,
      intensity => 1000, 
      period => 3600
     },        

    Children =
    [#{
      id => {'pnsvc_worker', N},   
      start => {'pnsvc_worker', start_link, []},      
      restart => permanent,   
      shutdown => 2000, 
      type => worker,       
      modules => []
     } || N <- lists:seq(1, WorkerNum)],

    {ok, {SupFlags, Children}}.
