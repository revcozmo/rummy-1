-module(rummy_endpoint).

-export([init/1,
         handle_call/2,
         handle_cast/2,
         handle_info/2,
         terminate/1]).

-record(state, {username, id, pid}).

%%%===================================================================
%%% jacket_handler callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({join, Username}, State) ->
    NewState = State#state{username=Username},
    {reply, rummy_lobby:join(Username), NewState};
handle_call(logout, State) ->
    {reply, rummy_lobby:logout(), State#state{id=undefined,pid=undefined}};
handle_call(get_rooms, State) ->
    {reply, rummy_lobby:rooms(), State};
handle_call(get_users, State) ->
    {reply, rummy_lobby:users(), State};
handle_call(create_room, State) ->
    handle_create_room(State);
handle_call({join_room, Id}, State) ->
    handle_join_room(Id, State);
handle_call(quit_room, #state{id=Id, pid=Pid, username=Username}=State) ->
    exit_room(Username, Id, Pid),
    {reply, {ok,ok}, State#state{id=undefined,pid=undefined}};
handle_call(start, #state{pid=Pid}=State) ->
    Reply = gen_fsm:sync_send_event(Pid, start),
    {reply, Reply, State};
handle_call(peek, #state{pid=Pid}=State) ->
    gen_fsm:sync_send_event(Pid, peek),
    {reply, {ok,ok}, State};
handle_call({put, List}, #state{pid=Pid}=State) ->
    Reply = gen_fsm:sync_send_event(Pid, {put, List}),
    {reply, Reply, State};
handle_call(_Request, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({lobby, _}=Msg, State) ->
    {reply, Msg, State};
handle_info({room, _}=Msg, State) ->
    {reply, Msg, State};
handle_info(_Msg, State) ->
    {noreply, State}.

terminate(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_join_room(Id, #state{id=Id}=State) ->
    {reply, {error, <<"Already there">>}, State};
handle_join_room(Id, #state{id=RoomId, username=Username, pid=Pid}=State) ->
    exit_room(Username, RoomId, Pid),
    case rummy_lobby:join_room(Id) of
        {ok, NewPid, Members} ->
            {reply, {ok,Members}, State#state{id=Id,pid=NewPid}};
        Other ->
            {reply, Other, State}
    end.

handle_create_room(#state{id=Id, username=Username, pid=Pid}=State) ->
    exit_room(Username, Id, Pid),
    case rummy_lobby:create_room() of
        {ok, NewPid, NewId} ->
            {reply, {ok,NewId}, State#state{id=NewId,pid=NewPid}};
        Other ->
            {reply, Other, State}
    end.

exit_room(_Username, _Id, undefined) ->
    ok;
exit_room(Username, Id, Pid) ->
    rummy_lobby:exit_room(Id, Username),
    gen_fsm:send_all_state_event(Pid, {exit_room, self()}).
