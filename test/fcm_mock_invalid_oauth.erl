-module(fcm_mock_invalid_oauth).

-export([init/2]).

init(Req0, State) ->
    Resp = jsone:encode(#{access_token => <<"test_invalid_access_token">>}),
    Req = cowboy_req:reply(200, #{}, Resp, Req0),
    {ok, Req, State}.
