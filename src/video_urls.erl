-module(video_urls).

-export([init/2]).
-export([content_types_provided/2]).
-export([to_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

% content_types_provided(Req, State) -> {Result, Req, State}
% Result     :: [{binary() | ParsedMime, ProvideCallback :: atom()}]
% ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params}
% Params     :: [{Key :: binary(), Value :: binary()}]
content_types_provided(Req, State) ->
    Handlers = [{{ <<"text">>, <<"json">>, '*'}, to_json}],
    {Handlers, Req, State}.

to_json(Req, State) ->
  Priv = code:priv_dir(how_erlang),
  Filename = Priv ++ "/urls.json",
  {ok, JSON} = file:read_file(Filename),
  {JSON, Req, State}.
