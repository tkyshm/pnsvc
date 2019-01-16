-module(pnsvc).

-export([send/2]).

-type response() :: binary().
-type project_id() :: iolist() | binary() | atom().

-spec send(pnsvc_fcm_request:request() | binary(), project_id()) -> {ok, integer(), response()} | {error, term()}.
send(Req, ProjectID) when is_atom(ProjectID)->
    send(Req, atom_to_list(ProjectID));
send(Req, ProjectID) when is_binary(ProjectID)->
    send(Req, binary_to_list(ProjectID));
send(Req, ProjectID) ->
    Pid = get_worker(),
    gen_server:call(Pid, {send, Req, ProjectID}).

% private
get_worker() ->
    Children = supervisor:which_children(pnsvc_worker_sup),
    N = (erlang:system_time(nanosecond) rem persistent_term:get(pnsvc_worker_num)) + 1,
    {_Id, Pid, _, _} = lists:nth(N, Children),
    Pid.
