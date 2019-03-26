-module(fcm_mock).

-export([start_link/0, init/2]).

start_link() -> 
    Dispatch = cowboy_router:compile([
        {'_', [{"/v1/projects/:project_id/messages/send", fcm_mock, []},
               {"/oauth", fcm_mock_oauth, []},
               {"/invalid_oauth", fcm_mock_invalid_oauth, []}
              ]}
    ]),

    Port = 28080,
    Args = [{port, Port}],
    Opts = #{env => #{dispatch => Dispatch}},

    cowboy:start_clear(fcm_mock_server, Args, Opts).

init(Req0, State) ->
    Req = handle_request(cowboy_req:method(Req0), Req0),
    timer:sleep(80 + rand:uniform(50)),
    {ok, Req, State}.

validate_access_token(Token) -> 
    case Token of
        <<"test_invalid_access_token">> -> throw({error, 401, invalid_access_token});
                                    _ -> ok
    end.

handle_request(<<"POST">>, Req0) ->
    try
        {bearer, Token} = cowboy_req:parse_header(<<"authorization">>, Req0),
        validate_access_token(Token),
        {ok, PostVals, Req} = cowboy_req:read_body(Req0),
        Data = jsone:decode(PostVals),
        {Data, Req}
    of
        {D, R} ->
            handle_data(D, R)
    catch
        _:{error, Status, Reason}:_Stack ->
            reply(Status, Reason, Req0)
    end;
handle_request(_Method, Req0) ->
    cowboy_req:reply(405, Req0).

handle_data(Data, Req0) ->
    HasMsg = maps:is_key(<<"message">>, Data),
    if 
        HasMsg =:= true -> 
            Msg = maps:get(<<"message">>, Data),
            HasToken = maps:is_key(<<"token">>, Msg),
            if 
                HasToken =:= true ->
                    case validate_token(maps:get(<<"token">>, Msg)) of
                        invalid_token ->
                             reply(400, invalid_token, Req0);
                        ok ->
                             reply(200, ok, Req0)
                    end;
                true ->
                    reply(400, no_message, Req0)
            end;
        true ->
            reply(400, no_message, Req0)
    end.

% defines several token for test.
validate_token(<<"invalid_token">>) -> invalid_token;
validate_token(_) -> ok.

reply(401, invalid_access_token, Req) ->
    ErrMsg =
    #{<<"error">> =>
      #{<<"code">> => 401,
        <<"message">> =>
        <<"Request had invalid authentication credentials. Expected OAuth 2 access token, login cookie or other valid authentication credential. See https://developers.google.com/identity/sign-in/web/devconsole-project.">>,
        <<"status">> => <<"UNAUTHENTICATED">>}},
    Resp = jsone:encode(ErrMsg),
    cowboy_req:reply(401, #{}, Resp, Req);
reply(400, no_message, Req) ->
    ErrMsg =
    #{<<"error">> =>
      #{<<"code">> => 400,
        <<"details">> =>
        [#{<<"@type">> =>
           <<"type.googleapis.com/google.rpc.BadRequest">>, 
           <<"fieldViolations">> =>
           [#{<<"description">> => 
              <<"Recipient of the message is not set.">>,
              <<"field">> => <<"message">>}]},
         #{<<"@type">> =>
           <<"type.googleapis.com/google.firebase.fcm.v1.FcmError">>,
           <<"errorCode">> => <<"INVALID_ARGUMENT">>}],
        <<"message">> => <<"Recipient of the message is not set.">>,
        <<"status">> => <<"INVALID_ARGUMENT">>}},

    Resp = jsone:encode(ErrMsg),
    cowboy_req:reply(400, #{}, Resp, Req);
reply(400, invalid_token, Req) ->
    ErrMsg = 
    #{<<"error">> =>
      #{<<"code">> => 400,
        <<"details">> =>
        [#{<<"@type">> =>
           <<"type.googleapis.com/google.rpc.BadRequest">>,
           <<"fieldViolations">> =>
           [#{<<"description">> =>
              <<"The registration token is not a valid FCM registration token">>,
              <<"field">> => <<"message.token">>}]},
         #{<<"@type">> =>
           <<"type.googleapis.com/google.firebase.fcm.v1.FcmError">>,
           <<"errorCode">> => <<"INVALID_ARGUMENT">>}],
        <<"message">> =>
        <<"The registration token is not a valid FCM registration token">>,
        <<"status">> => <<"INVALID_ARGUMENT">>}},

    Resp = jsone:encode(ErrMsg),
    cowboy_req:reply(400, #{}, Resp, Req);
reply(200, ok, Req) ->
    Headers = 
    #{
     <<"content-type">> => <<"application/json; charset=UTF-8">>,
     <<"date">> => <<"Thu, 17 Jan 2019 08:40:00 GMT">>,
     <<"server">> => <<"ESF">>,
     <<"cache-control">> => <<"private">>,
     <<"x-xss-protection">> => <<"1; mode=block">>,
     <<"x-frame-options">> => <<"SAMEORIGIN">>,
     <<"x-content-type-options">> => <<"nosniff">>,
     <<"alt-svc">> => <<"quic=\":443\"; ma=2592000; v=\"44,43,39,35\"">>,
     <<"accept-ranges">> => <<"none">>
    },
    Resp = <<"{\n  \"name\": \"projects/___/messages/0:1547714400087000%1900d0b61900d0b6\"\n}\n">>,
    cowboy_req:reply(200, Headers, Resp, Req).
