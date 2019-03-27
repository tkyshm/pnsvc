%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-01-17 17:55:57.653719
%%%-------------------------------------------------------------------
-module(pnsvc_SUITE).


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
         t_send/1,
         t_http_send/1,
         t_oauth_error/1
        ]).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%-include_lib("proper/include/proper.hrl").
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
        {test, [], [
            t_send,
            t_http_send,
            t_oauth_error
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
init_per_testcase(t_oauth_error, Config) ->
    Account = persistent_term:get(pnsvc_service_account),
    NewAccount = maps:put(<<"token_uri">>, <<"http://localhost:28080/invalid_oauth">>, Account),
    persistent_term:put(pnsvc_service_account, NewAccount),
    {ok, Size} = application:get_env(pnsvc, pool_size),
    [pnsvc_client_pool:refresh_client(Id) || Id <- lists:seq(1, Size)],
 
    [{service_account, Account}|Config];
init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(t_oauth_error, Config) ->
    Account = proplists:get_value(service_account, Config),
    persistent_term:put(pnsvc_service_account, Account),
    ok;
end_per_testcase(_TestCase, _Config) ->
    ok.

%%%===================================================================
%%% Individual Test Cases (from groups() definition)
%%%===================================================================

t_send(_Config) -> 
    TestTable = 
    [
     #{
       name => "success",
       status => 200, 
       request => #{message => #{token => <<"ok token">>}},
       err_type => <<>>,
       send_count => 1,
       err_count => 0
      },
     #{
       name => "message is invalid",
       status => 400,
       request => #{message => #{}},
       err_type => <<"type.googleapis.com/google.rpc.BadRequest">>,
       send_count => 1,
       err_count => 1
      },
     #{
       name => "token is invalid",
       status => 400,
       request => #{message => #{token => <<"invalid_token">>}},
       err_type => <<"type.googleapis.com/google.rpc.BadRequest">>,
       send_count => 1,
       err_count => 2
      }
    ],

    TestFun =
    fun(#{name := _TestName,
          status := WantStatus,
          err_type := WantErrType,
          request := Req,
          err_count := WantErrCnt,
          send_count := WantSendCount}) ->
        {ok, GotStatus, GotRawResp} = pnsvc:send(Req, "project-some-id"),
        GotResp = jsone:decode(GotRawResp),

        ?assertEqual(#{err_count => WantErrCnt, send_count => WantSendCount}, pnsvc_stats:stats()),

        ?assertEqual(WantStatus, GotStatus),
        if 
            WantStatus >= 300 ->
                GotErrType = get_err_type(GotResp),
                ?assertEqual(WantErrType, GotErrType);
            true ->
                ok
        end
    end,

    [TestFun(T) || T <- TestTable].

t_http_send(_Config) ->
    TestTable = 
    [
     #{
       name => "success",
       method => post,
       status => 200, 
       request => #{message => #{token => <<"ok token">>}},
       content_type => "application/json"
      },
     #{
       name => "message is invalid",
       method => post,
       status => 400,
       request => #{message => #{}},
       content_type => "application/json"
      },

     #{
       name => "token is invalid",
       method => post,
       status => 400,
       request => #{message => #{token => <<"invalid_token">>}},
       content_type => "application/json"
      },

     #{
       name => "unsupported media type",
       method => post,
       status => 415,
       request => #{message => #{token => <<"invalid_token">>}},
       content_type => "text/html"
      },
     #{
       name => "request body is empty",
       method => post,
       status => 400,
       request => undefined,
       content_type => "text/html"
      },
     #{
       name => "method not allow",
       method => get,
       status => 405, 
       request => #{message => #{token => <<"ok token">>}},
       content_type => "application/json"
      }
    ],

    URL = "http://localhost:25000/v1/projects/test_project/send",
    HttpcOpts = [{body_format, binary}],
    TestFun =
    fun(#{name := _Name, method := Method, request := Req, content_type := Type, status := Status}) ->
        Body = 
        case Req of
            undefined ->
                <<>>;
            _ ->
                jsone:encode(Req)
        end,

        Got =
        case Method of
            get ->
                HttpReq = {URL, []},
                {ok, {{_Proto, GotStatus, _OK}, _, _}} = httpc:request(Method, HttpReq, [], HttpcOpts),
                GotStatus;
            post ->
                HttpReq = {URL, [], Type, Body},
                {ok, {{_Proto, GotStatus, _OK}, _, _}} = httpc:request(Method, HttpReq, [], HttpcOpts),
                GotStatus
        end,

        ?assertEqual(Status, Got)
    end,

    [TestFun(T) || T <- TestTable].

t_oauth_error(_Config) ->
    TestTable = 
    [
     #{
       name => "invalid access token",
       status => 401, 
       request => #{message => #{token => <<"ok token">>}},
       resp_status => <<"UNAUTHENTICATED">>
      }
    ],

    TestFun =
    fun(#{name := _TestName, status := WantStatus, resp_status := WantRespStatus, request := Req}) ->
        {ok, GotStatus, GotRawResp} = pnsvc:send(Req, "project-some-id"),

        GotResp = maps:get(<<"error">>, jsone:decode(GotRawResp)),
        GotRespStatus = maps:get(<<"status">>, GotResp),

        ?assertEqual(WantStatus, GotStatus),
        ?assertEqual(WantRespStatus, GotRespStatus)
    end,

    [TestFun(T) || T <- TestTable].

% private
get_err_type(Resp) ->
    [Detail|_] = maps:get(<<"details">>, maps:get(<<"error">>, Resp)),
    maps:get(<<"@type">>, Detail).
