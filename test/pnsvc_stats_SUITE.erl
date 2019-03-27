%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-03-27 16:47:42.714960
%%%-------------------------------------------------------------------
-module(pnsvc_stats_SUITE).


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
         %% TODO: test case names go here
         t_incr_send_count/1,
         t_incr_err_count/1,
         t_stats/1
        ]).

%-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%-define(PROPTEST(M,F), true = proper:quickcheck(M:F())).

all() ->
    [
     %% TODO: Group names here e.g. {group, crud}
     {group, test}
    ].

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap, {seconds, 30}}].

groups() ->
    [
        %% TODO: group definitions here e.g.
        %% {crud, [], [
        %%          t_create_resource,
        %%          t_read_resource,
        %%          t_update_resource,
        %%          t_delete_resource
        %%         ]}
     {test, [], [
                 t_stats,
                 t_incr_err_count,
                 t_incr_send_count
                ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(cowboy),
    {ok, Pid} = fcm_mock:start_link(),
    [{fcm_mock, Pid}|Config].

end_per_suite(Config) ->
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
    {ok, _} = application:ensure_all_started(pnsvc),
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok = application:stop(pnsvc),
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

t_incr_send_count(_Config) -> 
    ok = pnsvc_stats:incr_send_count(),
    ?assertEqual(#{ send_count => 1, err_count => 0 }, pnsvc_stats:stats()).

t_incr_err_count(_Config) -> 
    ok = pnsvc_stats:incr_err_count(),
    ?assertEqual(#{ send_count => 0, err_count => 1 }, pnsvc_stats:stats()).

t_stats(_Config) -> 
    ?assertEqual(#{ send_count => 0, err_count => 0 }, pnsvc_stats:stats()).
