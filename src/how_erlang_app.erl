-module(how_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [{"/", cowboy_static, {priv_file, how_erlang, "static/how_erl.html"}},
               {"/assets/[...]", cowboy_static, {priv_dir, how_erlang, "static"}},
               {"/video_urls", video_urls, []}]}
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
