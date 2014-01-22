-module(rummy_user).

-export([login/2,
         register/2]).

-include("rummy.hrl").

register(Username, Password) ->
    case mnesia:dirty_read(user, Username) of
        [] ->
            User = #user{username = Username,
                         password = erlang:md5(Password)},
            mnesia:dirty_write(User),
            {ok, User};
        _ ->
            {error, <<"User already exists">>}
    end.

login(Username, Password) ->
    Md5 = erlang:md5(Password),
    case mnesia:dirty_read(user, Username) of
        [#user{password=Md5}=User] ->
            {ok, User#user{password=undefined}};
        _  ->
            {error, <<"Incorrect login">>}
    end.
