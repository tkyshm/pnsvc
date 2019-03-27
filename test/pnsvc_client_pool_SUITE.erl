%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-03-27 13:08:47.833270
%%%-------------------------------------------------------------------
-module(pnsvc_client_pool_SUITE).


%% API
-export([all/0,
         suite/0,
         groups/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([
         t_fetch/1,
         t_force_all_refresh/1,
         t_refresh_client/1
        ]).

%-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
     {test, [], [
                 t_fetch,
                 t_force_all_refresh,
                 t_refresh_client
                ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, Pid} = fcm_mock:start_link(),
    {ok, _} = application:ensure_all_started(pnsvc),
    [{fcm_mock, Pid}|Config].

end_per_suite(Config) ->
    ok = application:stop(pnsvc),
    ok = application:stop(ranch),
    ok = application:stop(cowboy),
    MockPid = proplists:get_value(fcm_mock, Config),
    exit(MockPid, kill),
    ok.

%%%===================================================================
%%% Group specific setup/teardown
%%%===================================================================
group(_Groupname) ->
    [].

init_per_group(_Groupname, Config) ->
    Config.

end_per_group(_Groupname, _Config) ->
    ok.

%%%===================================================================
%%% Testcase specific setup/teardown
%%%===================================================================
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================
t_fetch(_Config) ->
    % not exists id
    ?assertEqual({111111, undefined}, pnsvc_client_pool:fetch(111111)),

    {Id, _Client} = Ret = pnsvc_client_pool:fetch(),
    ?assertEqual(Ret, pnsvc_client_pool:fetch(Id)),

    {ok, DefaultSize} = application:get_env(pnsvc, pool_size),
    application:set_env(pnsvc, pool_size, 0),

    ok = application:stop(pnsvc),
    {ok, _} = application:ensure_all_started(pnsvc),

    ?assertEqual({error, err_pool_is_empty}, pnsvc_client_pool:fetch()),

    application:set_env(pnsvc, pool_size, DefaultSize),

    ok = application:stop(pnsvc),
    {ok, _} = application:ensure_all_started(pnsvc).

t_force_all_refresh(_Config) -> 
    {state, PoolSize, Clients} = sys:get_state(pnsvc_client_pool),
    pnsvc_client_pool:force_all_refresh(),
    {state, PoolSize, NewClients} = sys:get_state(pnsvc_client_pool),
    ?assertNotEqual(Clients, NewClients).

t_refresh_client(_Config) -> 
    {Id, Client} = pnsvc_client_pool:fetch(),
    pnsvc_client_pool:refresh_client(Id),
    {Id, NewClient} = pnsvc_client_pool:fetch(Id),
    ?assertNotEqual(Client, NewClient).

