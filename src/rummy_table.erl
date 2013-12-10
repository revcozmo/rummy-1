-module(rummy_table).

-behaviour(gen_fsm).

%% API
-export([start_link/3]).

%% gen_fsm callbacks
-export([init/1,
         initial/2,
         initial/3,
         play/2,
         play/3,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-record(player, {name, pid, role, tiles = []}).
-record(state, {id,
                limit = 60000,
                players=[],
                queue,
                sets,
                bank}).

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
                                               role=owner}]},
    erlang:monitor(process, Pid),
    {ok, initial, State}.

initial(_Event, State) ->
    {next_state, initial, State}.

initial({join, User, Pid}, _From, State) ->
    {Reply, NewState} = handle_join(User, Pid, State),
    {reply, Reply, initial, NewState};
initial(start, {Pid,_}, State) ->
    {Reply, NewStateName} = handle_start(Pid, State),
    {reply, Reply, NewStateName, State};
initial(_Event, _From, State) ->
    {reply, ok, initial, State}.

play(timeout, State=#state{queue=[Player|_]}) ->
    {next_state, play, State}.

play(peek, {Pid,_}, State) ->
    {reply, ok, play, State};
play({put, List}, {Pid,_}, State) ->
    {reply, ok, play, State}.

handle_event({exit_room, Pid}, StateName, State) ->
    handle_down(Pid, StateName, State);
handle_event(_Event, StateName, State) ->
    {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
    Reply = ok,
    {reply, Reply, StateName, State}.

handle_info(timeout, countdown, State=#state{limit=Limit}) ->
    NewState = start_game(State),
    {next_state, play, NewState, Limit};
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
handle_down(Pid, StateName, #state{players=Players, id=Id}=State) ->
    case is_owner(Pid, Players) of
        true ->
            broadcast(exit, State),
            {stop, normal, State};
        false ->
            NewPlayers = case lists:keyfind(Pid, #player.pid, Players) of
                #player{name=Username} ->
                    rummy_lobby:exit_room(Id, Username),
                    broadcast({exit, Username}, State),
                    lists:keydelete(Pid, #player.pid, Players);
                _ ->
                    Players
            end,
            {next_state, StateName, State#state{players=NewPlayers}}
    end.

handle_join(Name, From, #state{players=Players}=State) ->
    case length(Players)<?MAX_PLAYERS of
        true ->
            case user_exists(From, Players) of
                false ->
                    NewPlayers = [#player{name=Name,
                                          pid=From,
                                          role=player} | Players],
                    Reply = {ok,player_names(NewPlayers)},
                    erlang:monitor(process, From),
                    broadcast({join, Name}, State),
                    {Reply, State#state{players=NewPlayers}};
                true ->
                    {{error, <<"User already there">>}, State}
            end;
        false ->
            Reply = {error, <<"Room is full">>},
            {Reply, State}
    end.     

handle_start(Pid, #state{players=Players}=State) ->
    case length(Players)>1 of
        true ->
            case is_owner(Pid, Players) of
                true ->
                    erlang:send_after(5000, self(), timeout),
                    broadcast(start, State),
                    {ok, countdown};
                false ->
                    {error, <<"Player is not a table owner">>}
            end;
        false ->
            {error, <<"Not enough players">>}
    end.

start_game(State=#state{players=Players}) ->
    Deck = rummy_deck:shuffle(),
    {NewPlayers, Bank} = deal_tiles(Deck, Players, []),
    State#state{players=NewPlayers, bank=Bank}.

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

user_exists(Pid, Players) ->
    case lists:keyfind(Pid, #player.pid, Players) of
        #player{} -> true;
        _         -> false
    end.

broadcast(Message, #state{players=Players}) ->
    [Pid ! {room, Message} || #player{pid=Pid} <- Players].

player_names(Players) ->
    [Name || #player{name=Name} <- Players].
