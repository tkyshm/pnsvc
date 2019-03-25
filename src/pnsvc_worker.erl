%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-01-15 11:27:13.112111
%%%-------------------------------------------------------------------
-module(pnsvc_worker).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
    id :: non_neg_integer() | undefined,
    client :: pnsvc_fcm_client:client() | undefined
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason} |
%%                     {ok, State, {continue, Continue}}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, #state{}, {continue, try_fetch}}.

handle_continue(try_fetch, State) ->
    case pnsvc_client_pool:fetch() of
        {error, Reason} ->
            logger:warning("error: reason=~p", [Reason]),
            timer:sleep(1000),
            {noreply, State, {continue, try_fetch}};
        {Id, C} ->
            {noreply, State#state{client = C, id = Id}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({send, _Req, _ProjectID}, _From, #state{client = undefined} = State) ->
    {reply, {error, err_not_fetched_client}, State, {continue, try_fetch}};
handle_call({send, Req, ProjectID}, _From, #state{client = C, id = Id} = State) ->
    IsExpired = pnsvc_fcm_client:is_expired(C),

    NewState =
        case IsExpired of
            true -> 
                State#state{client = do_refresh_client(Id)};
            _ ->
                State
        end,

    try pnsvc_fcm_client:post(C, Req, ProjectID) of
        Reply ->
            logger:debug("reply: ~p", [Reply]),
            {reply, Reply, NewState}
    catch
        _:{error_post, ErrPostReason, _}:_ ->
            % reply pnsvc server error
            Reply = {error, ErrPostReason},
            case pnsvc_client_pool:refresh_client(Id) of
                {ok, NewC} -> 
                    {reply, Reply, NewState#state{client = NewC}, {continue, try_fetch}};

                {error, Reason} ->
                    logger:warning("refresh_client error: reason=~p", [Reason]),
                    {reply, Reply, NewState#state{client = undefined}, {continue, try_fetch}}
            end;

        _:Reason:Stack ->
            logger:warning("error: reason=~p stack=~p", [Reason, Stack]),
            {reply, Reason, NewState}
    end;
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, #state{client = C}) ->
    pnsvc_fcm_client:close(C),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% private
do_refresh_client(Id) ->
    case pnsvc_client_pool:refresh_client(Id) of
        {ok, NewC} -> 
            NewC;

        {error, Reason} ->
            logger:warning("refresh_client error: reason=~p", [Reason]),
            undefined
    end.
