-module(video_urls).

-export([init/2]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).
-export([to_json/2]).
-export([from_json/2]).

init(Req, State) ->
    {cowboy_rest, Req, State}.

allowed_methods(Req, State) ->
    AllowedMethods = [<<"GET">>, <<"HEAD">>, <<"OPTIONS">>, <<"POST">>],
    {AllowedMethods, Req, State}.

% content_types_provided(Req, State) -> {Result, Req, State}
% Result     :: [{binary() | ParsedMime, ProvideCallback :: atom()}]
% ParsedMime :: {Type :: binary(), SubType :: binary(), '*' | Params}
% Params     :: [{Key :: binary(), Value :: binary()}]
content_types_provided(Req, State) ->
    Handlers = [{{ <<"application">>, <<"json">>, '*'}, to_json}],
    {Handlers, Req, State}.

content_types_accepted(Req, State) ->
    ContentTypesAccepted = [{{<<"application">>, <<"json">>, '*'}, from_json}],
    {ContentTypesAccepted, Req, State}.

to_json(Req, State) ->
    Priv = code:priv_dir(how_erlang),
    Filename = Priv ++ "/urls.json",
    {ok, JSON} = file:read_file(Filename),
    {JSON, Req, State}.

% TODO prevent race conditions between read and write
from_json(Req0, State) ->
    {ok, JSON, Req1} = cowboy_req:read_body(Req0),
    Priv = code:priv_dir(how_erlang),
    Filename = Priv ++ "/urls.json",
    file:write_file(Filename, JSON),
    {true, Req1, State}.
