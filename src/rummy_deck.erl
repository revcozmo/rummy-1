-module(rummy_deck).

%% Public API
-export([deck/0,
         shuffle/0,
         shuffle/1,
         is_correct/1]).

-include("rummy.hrl").

%%%===================================================================
%%% Public functions
%%%===================================================================
deck() ->
    N = 13,
    Colors = [red, black, blue, orange],
    ColorsDup = [lists:duplicate(N, Color) || Color <- Colors],
    Numbers = lists:duplicate(length(Colors), lists:seq(1,13)),
    Zipped = lists:zip(lists:flatten(ColorsDup), lists:flatten(Numbers)),
    Regular = [#card{color=Color, number=Number} || {Color,Number} <- Zipped],
    Half = [#card{}|Regular],
    Half ++ Half.

shuffle() ->
    shuffle(deck()).

shuffle(Deck) ->
    random:seed(os:timestamp()),
    shuffle(Deck, length(Deck), []).

is_correct(Sets) ->
    lists:all(fun check_is_correct/1, Sets).

%%%===================================================================
%%% Internal functions
%%%===================================================================
shuffle([], 0, Acc) ->
    Acc;
shuffle(Deck, Remaining, Acc) ->
    Nth = lists:nth(random:uniform(Remaining), Deck),
    shuffle(lists:delete(Nth, Deck), Remaining-1, [Nth|Acc]).

check_is_correct(Set) when length(Set) < 3 ->
    false;
check_is_correct(Set) ->
    NoJokers = lists:filter(fun
                (#card{color=joker}) -> false;
                (#card{})            -> true
            end, Set),
    NoJokersLength = length(NoJokers),
    Colors = lists:foldl(fun(#card{color=Color}, Acc) ->
                    ordsets:add_element(Color, Acc)
            end, ordsets:new(), NoJokers),
    case ordsets:size(Colors) of
        1              -> check_sequence(Set, undefined);
        NoJokersLength -> check_equality(Set, undefined);
        _              -> false
    end.

check_sequence([], _) ->
    true;
check_sequence([#card{color=joker}|Rest], undefined) ->
    check_sequence(Rest, undefined);
check_sequence([#card{number=Number}|Rest], undefined) ->
    check_sequence(Rest, Number);
check_sequence([#card{color=joker}|Rest], Last) ->
    check_sequence(Rest, Last+1);
check_sequence([#card{number=Number}|Rest], Last) when Last+1 =:= Number ->
    check_sequence(Rest, Last+1);
check_sequence(_, _) ->
    false.

check_equality([], _) ->
    true;
check_equality([#card{color=joker}|Rest], undefined) ->
    check_equality(Rest, undefined);
check_equality([#card{number=Number}|Rest], undefined) ->
    check_equality(Rest, Number);
check_equality([#card{color=joker}|Rest], Number) ->
    check_equality(Rest, Number);
check_equality([#card{number=Number}|Rest], Number) ->
    check_equality(Rest, Number);
check_equality(_, _) ->
    false.
