-module(how_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    io:format("0~n"),

    Dispatch = cowboy_router:compile([
        {'_', [{"/", hello_handler, []}]}
    ]),

    io:format("1~n"),

    {ok, _} = cowboy:start_clear(how_erl_cowboy,
        [{port, 8080}],
        #{env => #{dispatch => Dispatch}}
    ),

    how_erlang_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
