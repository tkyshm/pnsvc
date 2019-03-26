-module(pnsvc_fcm_client).

-export([
    new/0,
    new/1,
    close/1,
    post/3,
    is_expired/1
]).

-record(client, {
    conn :: pid() | undefined,
    token :: binary() | undefined,
    expire = 0 :: non_neg_integer()
}).

-opaque client() :: #client{} | undefined.

-export_type([client/0]).

-ifdef(TEST).
-define(FCM_HOST, "localhost").
-define(FCM_PORT, 28080).
-define(FCM_ENDPOINT(ProjectID), lists:flatten(["/v1/projects/", ProjectID, "/messages/send"])).
-else.
-define(FCM_HOST, "fcm.googleapis.com").
-define(FCM_PORT, 443).
-define(FCM_ENDPOINT(ProjectID), lists:flatten(["/v1/projects/", ProjectID, "/messages:send"])).
%% https://fcm.googleapis.com/v1/projects/myproject-b5ae1/messages:send
-endif.

-define(DEFAULT_EXPIRE, 3600).
-define(HTTPC_OPTS, [{body_format, binary}]).
-define(KEEPALIVE, 5000).
-define(CONNECT_TIMEOUT, 60000).

-spec new() -> client().
new() -> new(?FCM_PORT).

-spec new(inet:port_number()) -> client().
new(Port) ->
    Opts = #{
      connect_timeout => ?CONNECT_TIMEOUT,
      http2_opts => #{ keepalive => ?KEEPALIVE},
      http_opts => #{ keepalive => ?KEEPALIVE}
     },
    case gun:open(?FCM_HOST, Port, Opts) of
        {ok, Conn} ->
            {ok, Protocol} = gun:await_up(Conn),
            logger:debug("protocol: ~p", [Protocol]),
            case fetch_token(#client{conn = Conn}) of
                {ok, C} ->
                    C;
                {error, Reason} ->
                    throw({err_new_fcm_client, Reason})
            end;

        {error, Reason} ->
            throw({err_new_fcm_client, Reason})
    end.

-spec post(client(), pnsvc_fcm_request:request(), iolist()) -> {ok, term(), term()}.
post(undefined, _Req, _ProjectID) ->
    throw({err_post, client_was_undefined});
post(#client{conn = Conn, token = Token}, Req, ProjectID) ->
    try 
        {ok, Body} = pnsvc_fcm_request:encode(Req),

        StreamRef = gun:post(Conn, ?FCM_ENDPOINT(ProjectID), [
            {<<"authorization">>, <<"Bearer ", Token/binary>>}
        ], Body),

        wait_body(Conn, StreamRef)
    catch
        _:{Reason, Status}:Stack ->
            logger:warning("post error: reason=~p status=~p stack=~p", [Reason, Status, Stack]),
            throw({err_post, Reason})
    end.

-spec close(client()) -> ok.
close(#client{conn = Conn}) -> gun:close(Conn).

-spec is_expired(client()) -> boolean().
is_expired(#client{expire = Expire}) ->
    Now = os:system_time(second),
    Expire < Now.

%% private
-define(TRY_COUNT, 3).
wait_body(Conn, StreamRef) -> wait_body(Conn, StreamRef, 0).

wait_body(_Conn, _StreamRef, N) when N =:= ?TRY_COUNT ->
    throw({error, err_timeout_wait_body});
wait_body(Conn, StreamRef, N) ->
    case gun:await(Conn, StreamRef) of
        {response, fin, Status, _Headers} ->
            throw({no_response_data, Status});

        {response, nofin, Status, Headers} ->
            logger:debug("headers: ~p", [Headers]),
            {ok, Body} = gun:await_body(Conn, StreamRef),
            {ok, Status, Body};

        {error, timeout} ->
            logger:info("error: timeout, retry wait_body(try=~p)", [N]),
            wait_body(Conn, StreamRef, N+1);

        {error, Reason} ->
            throw({Reason, 500})
    end.

fetch_token(#client{expire = OldExpire} = C) ->
    Now = erlang:system_time(seconds),
    case OldExpire - 60 =< Now of
        true ->
            case do_refresh_token(Now) of
                {error, {ErrMsg, Reason, Stack}} ->
                    logger:warning("~p: reason=~p stack=~p", [ErrMsg, Reason, Stack]),
                    {error, Reason};

                {Token, Expire} ->
                    {ok, C#client{token = Token, expire = Expire}}
            end;

        false ->
            {ok, C}
    end.

do_refresh_token(Now) ->
    try 
        Account = persistent_term:get(pnsvc_service_account),
        TokenUrl = binary_to_list(maps:get(<<"token_uri">>, Account)),
        {Assertion, Expire} = generate_token(Now),

        Body = jsone:encode(#{
            assertion => Assertion,
            grant_type => <<"urn:ietf:params:oauth:grant-type:jwt-bearer">>
        }),

        case httpc:request(post, {TokenUrl, [], "application/json", Body}, [], ?HTTPC_OPTS) of
            {ok, {{_Protocol, 200, _OK}, _RespHeaders, RespJson}} ->
                Resp = jsone:decode(RespJson),
                AccessToken = maps:get(<<"access_token">>, Resp),
                {AccessToken, Expire};
            Ret ->
                logger:warning("ret: ~p", [Ret]),
                {error, {err_do_refresh_token_failed, Ret, []}}
        end
    of
        Result -> Result
    catch
        _:Reason:Stack ->
            logger:warning("reason: ~p", [Reason]),
            {error, {err_do_refresh_token_failed, Reason, Stack}}
    end.

generate_token(Now) ->
    Account = persistent_term:get(pnsvc_service_account),

    ClientEmail = maps:get(<<"client_email">>, Account),
    Expire = Now + ?DEFAULT_EXPIRE,
    Claims = 
    #{
      <<"iat">>    => Now,
      <<"exp">>    => Expire,
      <<"iss">>    => ClientEmail,
      <<"sub">>    => ClientEmail,
      <<"aud">>    => <<"https://oauth2.googleapis.com/token">>,
      <<"scope">>  => <<"https://www.googleapis.com/auth/firebase.messaging">>
     },

    PrivateKey = maps:get(<<"private_key">>, Account),
    JWK = jose_jwk:from_pem(PrivateKey),
    KID = maps:get(<<"private_key_id">>, Account),
    JWS = #{
      <<"alg">> => <<"RS256">>,
      <<"typ">> => <<"JWT">>,
      <<"kid">> => KID
     },
    Signed = jose_jwt:sign(JWK, JWS, Claims),
    {_JWS, Token} = jose_jws:compact(Signed),

    {Token, Expire}.
