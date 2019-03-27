%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-03-26 19:41:40.583790
%%%-------------------------------------------------------------------
-module(pnsvc_fcm_client_SUITE).


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
         t_new/1,
         t_post/1,
         t_close/1,
         t_is_expired/1
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
                 t_new,
                 t_post,
                 t_close,
                 t_is_expired
                ]}
    ].

%%%===================================================================
%%% Overall setup/teardown
%%%===================================================================
init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(gun),
    {ok, _} = application:ensure_all_started(cowboy),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(gun),
    ok = application:stop(ranch),
    ok = application:stop(cowboy),
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
init_per_testcase(t_new, Config) ->
    Config;
init_per_testcase(_TestCase, Config) ->
    {ok, _} = application:ensure_all_started(ranch),
    {ok, Pid} = fcm_mock:start_link(),
    [{fcm_mock, Pid}|Config].

end_per_testcase(t_new, _Config) ->
    ok;
end_per_testcase(_TestCase, Config) ->
    MockPid = proplists:get_value(fcm_mock, Config),
    exit(MockPid, kill),
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

t_new(_Config) ->
    ?assertException(throw, {err_new_fcm_client, timeout}, pnsvc_fcm_client:new()),

    {ok, MockPid} = fcm_mock:start_link(),

    ?assertMatch({client, _, <<"test_access_token">>, _}, pnsvc_fcm_client:new()),

    exit(MockPid, kill).

t_post(_Config) ->
    ProjectId = "project-some-id",
    Req = 
      #{ 
        message => #{
          token => <<"token">>
        }
      },

    ?assertException(throw, {err_post, client_was_undefined}, pnsvc_fcm_client:post(undefined, Req, ProjectId)),

    C = pnsvc_fcm_client:new(),

    {ok, GotStatus, GotRawResp} = pnsvc_fcm_client:post(C, Req, ProjectId),
    GotResp = jsone:decode(GotRawResp),
    ?assertEqual(200, GotStatus),
    ?assert(maps:is_key(<<"name">>, GotResp)).

t_close(_Config) ->
    C = pnsvc_fcm_client:new(),
    ?assertEqual(ok, pnsvc_fcm_client:close(C)).

t_is_expired(_Config) ->
    DefaultExpire = persistent_term:get(pnsvc_access_token_expires),
    persistent_term:put(pnsvc_access_token_expires, 0),

    C = pnsvc_fcm_client:new(),

    ?assert(pnsvc_fcm_client:is_expired(C) == false),

    timer:sleep(1000),

    ?assert(pnsvc_fcm_client:is_expired(C)),

    persistent_term:put(pnsvc_access_token_expires, DefaultExpire).
