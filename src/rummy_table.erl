-module(rummy_table).

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
         initial/2,
         initial/3,
         countdown/2,
         countdown/3,
         play/2,
         play/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(player, {name, pid, role, tiles = [], ready, initial=false}).
-record(state, {id,
                limit = 60000,
                players=[],
                queue,
                sets,
                bank,
                timer}).

-define(MAX_PLAYERS, 4).
-include("rummy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(Id, Name, Pid) ->
    gen_fsm:start_link(?MODULE, [Id, Name, Pid], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Id, Name, Pid]) ->
    State = #state{id = Id, players = [#player{name=Name,
                                               pid=Pid,
                                               ready=false,
                                               role=owner}]},
    erlang:monitor(process, Pid),
    {ok, initial, State}.

initial(_Event, State) ->
    {next_state, initial, State}.

initial({join, User, Pid}, _From, State) ->
    {Reply, NewState} = handle_join(User, Pid, State),
    {reply, Reply, initial, NewState};
initial(start, {Pid,_}, State) ->
    {NewStateName, NewState} = handle_start(Pid, State),
    {reply, {ok,ok}, NewStateName, NewState};
initial(_Event, _From, State) ->
    {reply, ok, initial, State}.

countdown(_Event, State) ->
    {next_state, countdown, State}.

countdown(_Event, _From, State) ->
    {reply, {error, <<"Already in play">>}, countdown, State}.

play(_Event, State) ->
    {next_state, play, State}.

play(peek, {Pid,_}, State=#state{timer=Timer, limit=Limit}) ->
    NewState = peek(Pid, State),
    erlang:cancel_timer(Timer),
    NewTimer = erlang:send_after(Limit, self(), timeout),
    {reply, ok, play, NewState#state{timer=NewTimer}};
play({put, List}, {Pid,_}, State) ->
    {Reply, NewStateName, NewState} = put(Pid, List, State),
    {reply, Reply, NewStateName, NewState};
play(_, _, State) ->
    {reply, {error, <<"Already in play">>}, play, State}.

handle_event({exit_room, Pid}, StateName, State) ->
    handle_down(Pid, StateName, State);
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(timeout, countdown, State=#state{queue=[Player|_], limit=Limit}) ->
    broadcast({move, Player#player.name#user.username}, State),
    Timer = erlang:send_after(Limit, self(), timeout), 
    {next_state, play, State#state{timer=Timer}};
handle_info(timeout, play, State=#state{limit=Limit}) ->
    NewState = peek(undefined, State),
    Timer = erlang:send_after(Limit, self(), timeout),
    {next_state, play, NewState#state{timer=Timer}};
handle_info({'DOWN', _, _, Pid, _}, StateName, State) ->
    handle_down(Pid, StateName, State);
handle_info(_Info, StateName, State) ->
    {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

%%%===================================================================
%%% Internal functions
%%%=================================================================== 
put(Pid1, _, #state{queue=[#player{pid=Pid2}|_]}=State) when Pid1 =/= Pid2 ->
    {{error, <<"Not your turn">>}, play, State};
put(_Pid, List, #state{queue=[Player|_]=Queue,sets=OldSets,
                       timer=Timer,limit=Limit,players=Players}=State) ->
    NewSets = proplist_to_tiles(List), 
    case is_play_legal(NewSets, OldSets, Player) of
        false ->
            {{error, <<"Illegal play">>}, play, State};
        {true, NewPlayer} ->
            broadcast({set, List}, State),
            NewQueue = lists:keystore(Player#player.name, #player.name,
                                      Queue, NewPlayer),
            NewState = rotate_queue(State#state{queue=NewQueue}),
            NewState1 = NewState#state{sets=NewSets},
            erlang:cancel_timer(Timer),
            NewTimer = erlang:send_after(Limit, self(), timeout),
            NewState2 = NewState1#state{timer=NewTimer},
            case NewPlayer#player.tiles of
                [] ->
                    NewPlayers = [P#player{ready=false} || P <- Players],
                    NewPlayers1 = update_ranks(NewPlayer, NewPlayers),
                    NewUsers = [U#player.name || U <- NewPlayers1],
                    broadcast({winner, NewPlayer#player.name#user.username,
                               user_to_proplist(NewUsers)}, State),
                    rummy_lobby:update_ranks(NewUsers),
                    {{ok,ok}, initial, NewState2#state{players=NewPlayers1}};
                _ ->
                    {{ok,ok}, play, NewState2}
            end
    end. 

is_play_legal(NewSets, OldSets, #player{tiles=Tiles, initial=Initial}=Player) ->
    DiffSets = lists:flatten(NewSets) -- lists:flatten(OldSets),
    DiffTiles = Tiles -- DiffSets,
    Sum = lists:foldl(fun(#card{number=N},Acc) -> N+Acc end, 0, DiffSets),
    case (not Initial andalso Sum < 30) of
        true ->
            false;
        _ ->
            case rummy_deck:is_correct(NewSets)
                andalso length(DiffSets) > 0
                andalso length(DiffTiles) =:= length(Tiles)-length(DiffSets) of
                true ->
                    {true, Player#player{tiles=DiffTiles, initial=true}};
                false ->
                    false
            end
    end.

peek(_, #state{bank=[]}=State) ->
    rotate_queue(State);
peek(Pid1, #state{queue=[#player{pid=Pid2}|_]}=State)
        when Pid1 =/= Pid2, is_pid(Pid1) ->
    %% ignore, not this guy's turn
    State; 
peek(_, #state{queue=[Player|_]=Queue, bank=[Tile|Bank]}=State) ->
    #player{pid=Pid, name=Name, tiles=Tiles} = Player,
    Pid ! {room, {tile, tiles_to_proplist(Tile)}},
    NewTiles = [Tile|Tiles],
    NewQueue = lists:keystore(Name, #player.name, Queue,
                              Player#player{tiles=NewTiles}),
    rotate_queue(State#state{queue=NewQueue, bank=Bank}).

rotate_queue(#state{queue=[Player,Next|Queue]}=State) ->
    NewQueue = [Next|Queue] ++ [Player],
    broadcast({move, Next#player.name#user.username}, State),
    State#state{queue=NewQueue}.

handle_down(Pid, _StateName, #state{players=Players, id=Id}=State) ->
    case is_owner(Pid, Players) of
        true ->
            broadcast(exit, State),
            {stop, shutdown, State};
        false ->
            NewPlayers = case lists:keyfind(Pid, #player.pid, Players) of
                #player{name=Username} ->
                    rummy_lobby:exit_room(Id, Username),
                    broadcast({exit, Username#user.username}, State),
                    lists:keydelete(Pid, #player.pid, Players);
                _ ->
                    Players
            end,
            {next_state, initial, State#state{players=NewPlayers}}
    end.

handle_join(Name, From, #state{players=Players}=State) ->
    case length(Players)<?MAX_PLAYERS of
        true ->
            case user_exists(Name, From, Players) of
                false ->
                    NewPlayers = [#player{name=Name,
                                          pid=From,
                                          ready=false,
                                          role=player} | Players],
                    Reply = {ok,player_names(NewPlayers)},
                    erlang:monitor(process, From),
                    broadcast({join, user_to_proplist(Name)}, State),
                    {Reply, State#state{players=NewPlayers}};
                true ->
                    {{error, <<"User already there">>}, State}
            end;
        false ->
            Reply = {error, <<"Room is full">>},
            {Reply, State}
    end.     

handle_start(Pid, #state{players=Players}=State) ->
    Player = lists:keyfind(Pid, #player.pid, Players),
    NewPlayers = lists:keystore(Pid, #player.pid, Players, Player#player{ready=true}),
    broadcast({ready, Player#player.name#user.username}, State),
    error_logger:info_msg("User ~p ready", [Player]),
    case are_all_players_ready(NewPlayers) of
        true ->
            erlang:send_after(5000, self(), timeout),
            NewState = start_game(State),
            send_tiles(NewState),
            error_logger:info_msg("All players ready, starting"),
            {countdown, NewState#state{players=NewPlayers}};
        _ ->
            {initial, State#state{players=NewPlayers}}
    end.

start_game(State=#state{players=Players}) ->
    Deck = rummy_deck:shuffle(),
    {NewPlayers, Bank} = deal_tiles(Deck, Players, []),
    NewPlayers1 = [Player#player{initial=false} || Player <- NewPlayers],
    State#state{players=NewPlayers1, bank=Bank, queue=NewPlayers, sets=[]}.

deal_tiles(Deck, [], Players) ->
    {lists:reverse(Players), Deck};
deal_tiles(Deck, [Player|Rest], Players) ->
    {Tiles, DeckRest} = lists:split(14, Deck),
    deal_tiles(DeckRest, Rest, [Player#player{tiles=Tiles}|Players]).

is_owner(Pid, Players) ->
    case lists:keyfind(Pid, #player.pid, Players) of
        #player{role=owner} -> true;
        _                   -> false
    end.

user_exists(Name, Pid, Players) ->
    case lists:keyfind(Name, #player.name, Players) of
        #player{} -> true;
        _ ->
            case lists:keyfind(Pid, #player.pid, Players) of
                #player{} -> true;
                _         -> false
            end
    end.

broadcast(Message, #state{players=Players}) ->
    [Pid ! {room, Message} || #player{pid=Pid} <- Players].

player_names(Players) ->
    [Name || #player{name=Name} <- Players].

are_all_players_ready(Players) ->
    Ready = lists:filter(fun
            (#player{ready=true}) -> true;
            (_)                   -> false
        end, Players),
    Length = length(Players),
    length(Ready) =:= Length andalso Length>1.

send_tiles(#state{players=Players}) ->
    [Pid ! {room, {start, tiles_to_proplist(Tiles)}} ||
        #player{pid=Pid, tiles=Tiles} <- Players]. 

proplist_to_tiles([{_,_}|_]=Proplist) ->
    {color, Color} = lists:keyfind(color, 1, Proplist),
    {number, Number} = lists:keyfind(number, 1, Proplist),
    #card{color = Color, number = Number};
proplist_to_tiles(List) when is_list(List) ->
    [proplist_to_tiles(Proplist) || Proplist <- List].

tiles_to_proplist(List) when is_list(List) ->
    [tiles_to_proplist(Tile) || Tile <- List];
tiles_to_proplist(#card{color=Color, number=Number}) ->
    [{color, Color}, {number, Number}].

user_to_proplist(List) when is_list(List) ->
    [user_to_proplist(User) || User <- List];
user_to_proplist(#user{username=Username, rank=Rank}) ->
    [{username, Username}, {rank, Rank}].

update_ranks(Winner, All) ->
    lists:foldr(fun
            (Player, Acc) when Player#player.name =:= Winner#player.name ->
                NewRank = count_new_rank(Player, All--[Player], 1),
                NewUser = set_new_rank(NewRank, Player#player.name#user.username),
                [Player#player{name=NewUser} | Acc];
            (Player, Acc) ->
                NewRank = count_new_rank(Player, All--[Player], 0),
                NewUser = set_new_rank(NewRank, Player#player.name#user.username),
                [Player#player{name=NewUser} | Acc]
        end, [], All).

set_new_rank(Rank, User) ->
    F = fun() ->
            [U] =  mnesia:read(user, User),
            NewUser =  U#user{rank=erlang:round(Rank)},
            mnesia:write(NewUser),
            NewUser
    end,
    {atomic, Ret} = mnesia:transaction(F),
    Ret.

count_new_rank(User, Others, Win) when is_list(Others) ->
    lists:sum([count_new_rank(User, Other, Win) || Other <- Others]) /
    length(Others);
count_new_rank(#player{name=#user{rank=X}},
               #player{name=#user{rank=Y}}, Win) ->
    D = Y-X,
    W = 1/(1+math:pow(10,D/400)),
    Diff = Win - W,
    K = case X of
        K1 when K1 < 2100 -> 32;
        K2 when K2 > 2400 -> 16;
        _ -> 24
    end,
    X + K*Diff.
