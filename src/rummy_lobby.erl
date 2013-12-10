-module(rummy_lobby).

-behaviour(gen_server).

%% API
-export([start_link/0,
         join/1,
         users/0,
         rooms/0,
         create_room/0,
         join_room/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {users, rooms}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Username) ->
    gen_server:call(?MODULE, {join, Username}).

users() ->
    gen_server:call(?MODULE, users).

rooms() ->
    gen_server:call(?MODULE, rooms).

create_room() ->
    gen_server:call(?MODULE, create_room).

join_room(Id) ->
    gen_server:call(?MODULE, {join_room, Id}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{users = [], rooms = []}}.

handle_call({join, Username}, {Pid, _}, State) ->
    {Reply, NewState} = handle_join(Username, Pid, State),
    {reply, Reply, NewState};
handle_call(users, _From, #state{users=Users}=State) ->
    Reply = {ok, [Username || {Username, _} <- lists:keysort(1, Users)]},
    {reply, Reply, State};
handle_call(rooms, _From, State) ->
    Reply = handle_rooms(State),
    {reply, Reply, State};
handle_call(create_room, {Pid, _}, State) ->
    {Reply, NewState} = handle_create_room(Pid, State),
    {reply, Reply, NewState};
handle_call({join_room, Id}, {Pid, _}, State) ->
    {Reply, NewState} = handle_join_room(Id, Pid, State),
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({'DOWN', _, _, Pid, _}, State) ->
    State1 = handle_down(Pid, State),
    {noreply, State1};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
handle_join(Username, Pid, #state{users=Users}=State) ->
    case lists:keyfind(Username, 1, Users) of
        {Username, _} ->
            error_logger:info_msg("User ~p tried to join. It exists, though", [Username]),
            {{error, <<"User exists">>}, State};
        _ ->
            error_logger:info_msg("User ~p joined", [Username]),
            erlang:monitor(process, Pid),
            NewUsers = lists:keystore(Pid, 2, Users, {Username,Pid}),
            broadcast({add_user, Username}, State),
            {{ok,ok}, State#state{users=NewUsers}}
    end.

handle_rooms(#state{rooms=Rooms}) ->
    [[{members, Members}, {id, Id}] || {_Pid, Id, Members} <- Rooms].

handle_create_room(Pid, State=#state{users=Users, rooms=Rooms}) ->
    case lists:keyfind(Pid, 2, Users) of
        {Username, Pid} ->
            {ok, TablePid} = supervisor:start_child(rummy_table_sup, [Username]),
            erlang:monitor(process, TablePid),
            Id = list_to_binary(uuid:to_string(uuid:v4())),
            NewRooms = [{TablePid, Id, [Username]} | Rooms],
            {{ok, TablePid}, State#state{rooms=NewRooms}};
        _ ->
            {{error, <<"No such user">>}, State}
    end.

handle_join_room(Id, Pid, State=#state{users=Users}) ->
    case lists:keyfind(Pid, 2, Users) of
        {Username, Pid} ->
            handle_join_room1(Id, Username, State);
        _ ->
            {{error, <<"No such user">>}, State}
    end.

handle_join_room1(Id, Username, State=#state{rooms=Rooms}) ->
    case lists:keyfind(Id, 2, Rooms) of
        {_, Id, _}=Room ->
            handle_join_room2(Room, Username, State);
        _ ->
            {{error, <<"No such room">>}, State}
    end.

handle_join_room2({RoomPid,Id,Members}, Username, State=#state{rooms=Rooms}) ->
    case gen_fsm:sync_send_event(RoomPid, {join, Username}) of
        {ok, ok} ->
            Room = {RoomPid, Id, [Username|Members]},
            NewRooms = lists:keystore(RoomPid, 1, Rooms, Room),
            {{ok,ok}, State#state{rooms = NewRooms}};
        Other ->
            {Other, State}
    end.

handle_down(Pid, State=#state{rooms=Rooms, users=Users}) ->
    NewRooms = case lists:keyfind(Pid, 1, Rooms) of
        {Pid, Id, _} ->
            error_logger:info_msg("Room ~p deleted", [Id]),
            broadcast({delete_room, Id}, State),
            lists:keydelete(Pid, 1, Rooms);
        _ ->
            Rooms
    end,
    NewUsers = case lists:keyfind(Pid, 2, Users) of
        {Username, Pid} ->
            error_logger:info_msg("User ~p deleted", [Username]),
            broadcast({delete_user, Username}, State),
            lists:keydelete(Pid, 2, Users);
        _ ->
            Users
    end,
    State#state{rooms=NewRooms, users=NewUsers}.

broadcast(Message, #state{users=Users}) ->
    [Pid ! {lobby, Message} || {_, Pid} <- Users].
