-module(rummy_table).

-behaviour(gen_fsm).

%% API
-export([start_link/2]).

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
-record(state, {limit = 60000,
                players=[],
                queue,
                sets,
                bank}).

-define(MAX_PLAYERS, 4).
-include("rummy.hrl").

%%%===================================================================
%%% API
%%%===================================================================
start_link(Name, Pid) ->
    gen_fsm:start_link(?MODULE, [Name, Pid], []).

%%%===================================================================
%%% gen_fsm callbacks
%%%===================================================================
init([Name, Pid]) ->
    State = #state{players = [#player{name=Name,
                                      pid=Pid,
                                      role=owner}]},
    erlang:monitor(process, Pid),
    {ok, initial, State}.

initial(_Event, State) ->
    {next_state, initial, State}.

initial({join, User}, {Pid,_}, State) ->
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
handle_down(Pid, StateName, #state{players=Players}=State) ->
    case is_owner(Pid, Players) of
        true ->
            {stop, <<"Owner exited">>, State};
        false ->
            %% todo
            {next_state, StateName, State}
    end.

handle_join(Name, From, #state{players=Players}=State) ->
    case length(Players)<?MAX_PLAYERS of
        true ->
            Reply = {ok,ok},
            Player = #player{name=Name,pid=From,role=player},
            NewState = State#state{players=[Player|Players]},
            {Reply, NewState};
        false ->
            Reply = {error, <<"Room is full">>},
            {Reply, State}
    end.     

handle_start(Pid, #state{players=Players}) ->
    case length(Players)>1 of
        true ->
            case is_owner(Pid, Players) of
                true ->
                    send_start_to_players(Players),
                    {ok, countdown};
                false ->
                    {error, <<"Player is not a table owner">>}
            end;
        false ->
            {error, <<"Not enough players">>}
    end.

send_start_to_players(Players) ->
    [Pid ! start || #player{pid=Pid} <- Players],
    erlang:send_after(5000, self(), timeout).

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
