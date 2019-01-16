-module(pnsvc_v1_send_handler).

-export([init/2]).

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    HasBody = cowboy_req:has_body(Req0),
    ContentType = cowboy_req:header(<<"content-type">>, Req0),
    Req = handle_request(Method, HasBody, ContentType, Req0),
    {ok, Req, State}.

handle_request(<<"POST">>, true, <<"application/json">>, Req0) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    ProjectID = cowboy_req:binding(project_id, Req1),
    case pnsvc:send(Body, ProjectID) of
        {ok, Status, Resp} ->
            cowboy_req:reply(Status, #{}, Resp, Req1);
        {error, Reason} ->
            logger:warning("error: reason=~p", [Reason]),
            cowboy_req:reply(500, #{}, <<"{\"error\": \"internal server error\"}">>, Req1)
    end;
handle_request(<<"POST">>, false, _, Req0) ->
    cowboy_req:reply(400, #{}, <<"{\"error\": \"request body was empty\"}">>, Req0);
handle_request(<<"POST">>, _, _, Req0) ->
    cowboy_req:reply(415, #{}, <<"{\"error\": \"unsupported media type\"}">>, Req0);
handle_request(_, _, _, Req0) ->
    cowboy_req:reply(405, #{}, <<"{\"error\": \"method not allow\"}">>, Req0).
