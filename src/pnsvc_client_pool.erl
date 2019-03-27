%%%-------------------------------------------------------------------
%%% @author tkyshm
%%% @copyright (C) 2019, tkyshm
%%% @doc
%%%
%%% @end
%%% Created : 2019-01-16 10:41:11.323302
%%%-------------------------------------------------------------------
-module(pnsvc_client_pool).

-behaviour(gen_server).

%% API
-export([start_link/1, fetch/0, fetch/1, force_all_refresh/0, refresh_client/1]).

%% gen_server callbacks
-export([init/1,
         handle_continue/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(SERVER, ?MODULE).

-type id() :: non_neg_integer().

-record(state, {
    pool_size = 5 :: non_neg_integer(),
    clients = [] :: [{id(), pnsvc_fcm_client:client()}]
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
start_link(Size) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Size], []).

-spec fetch() -> {id(), pnsvc_fcm_client:client()} | {error, term()}.
fetch() ->
    gen_server:call(?SERVER, fetch).

-spec fetch(id()) -> {id(), pnsvc_fcm_client:client()} | {error, term()}.
fetch(Id) ->
    gen_server:call(?SERVER, {fetch, Id}).

-spec force_all_refresh() -> ok | {error, term()}.
force_all_refresh() ->
    gen_server:call(?SERVER, init_connect).

-spec refresh_client(id()) -> {ok, pnsvc_fcm_client:client()} | {error, term()}.
refresh_client(Id) ->
    gen_server:call(?SERVER, {refresh_client, Id}).

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
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Size]) ->
    {ok, #state{pool_size = Size}, {continue, init_connect}}.

handle_continue(init_connect, #state{pool_size = Size} = State) ->
    try
        [{Id, try_connect()} || Id <- lists:seq(1, Size)]
    of
        Clients -> {noreply, State#state{clients = Clients}}
    catch
        _:Reason:Stack ->
            logger:warning("error: reason=~p stack=~p", [Reason, Stack]),
            timer:sleep(1000),
            {noreply, State, {continue, init_connect}}
    end;
handle_continue({close, Client}, State) ->
    pnsvc_fcm_client:close(Client),
    {noreply, State}.

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
handle_call({refresh_client, Id}, _From, #state{clients = OldClients} = State) ->
    Client = proplists:get_value(Id, OldClients),
    NewClients = proplists:delete(Id, OldClients),

    try try_connect() of
        NewClient ->
            NewState = State#state{clients = [{Id, NewClient} | NewClients]},
            {reply, {ok, NewClient}, NewState, {continue, {close, Client}}}
    catch
        _:Reason:Stack ->
            logger:warning("error: reason=~p stack=~p", [Reason, Stack]),
            {reply, {error, Reason}, State#state{clients = NewClients}}
    end;
handle_call(init_connect, _From, #state{clients = OldClients, pool_size = Size} = State) ->
    % avoid to leak processes.
    close_clients(OldClients),

    try init_connect(Size) of
        Clients ->
            {reply, ok, State#state{clients = Clients}}
    catch
        _:Reason:Stack ->
            logger:warning("error: reason=~p stack=~p", [Reason, Stack]),
            {reply, {error, Reason}, State#state{clients = []}, {continue, init_connect}}
    end;
handle_call(fetch, _From, #state{clients = []} = State) ->
    {reply, {error, err_pool_is_empty}, State};
handle_call(fetch, _From, #state{clients = [H|Clients]} = State) ->
    NewClients = lists:reverse([H|lists:reverse(Clients)]),
    {reply, H, State#state{clients = NewClients}};
handle_call({fetch, Id}, _From, #state{clients = Clients} = State) ->
    Client = proplists:get_value(Id, Clients),
    {reply, {Id, Client}, State};
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
terminate(_Reason, _State) ->
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

try_connect() ->
    try
        pnsvc_fcm_client:new()
    of
        C -> C
    catch
        _:Reason:Stack ->
            throw({err_try_connect, Reason, Stack})
    end.

init_connect(Size) ->
    try
        [{Id, try_connect()} || Id <- lists:seq(1, Size)]
    of
        Pids -> Pids
    catch
        _:Reason:Stack ->
            throw({err_init_connect, Reason, Stack})
    end.

close_clients(Clients) ->
    [pnsvc_fcm_client:close(Client) || {_Id, Client} <- Clients].
