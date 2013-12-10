-module(rummy_endpoint).

-export([init/1,
         handle_call/2,
         handle_cast/2,
         handle_info/2,
         terminate/1]).

-record(state, {}).

%%%===================================================================
%%% jacket_handler callbacks
%%%===================================================================
init([]) ->
    {ok, #state{}}.

handle_call({join, Username}, State) ->
    {reply, rummy_lobby:join(Username), State};
handle_call(logout, State) ->
    {reply, rummy_lobby:logout(), State};
handle_call(get_rooms, State) ->
    {reply, rummy_lobby:rooms(), State};
handle_call(get_users, State) ->
    {reply, rummy_lobby:users(), State};
handle_call(create_room, State) ->
    {reply, rummy_lobby:create_room(), State};
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
