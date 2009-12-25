%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc start point to use Restfuilerl.

-module(restfulierl).
-author('Leandro Silva <leandrodoze@gmail.com>').

%% operation & maintenance api
-export([start/0, stop/0]).

%% external api
-export([from_web/1]).

-include("restfulierl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%
%% Operation & Maintenance API
%%

%% @spec start() -> ok
%% @doc Start the Restfulierl application and dependencies.
start() ->
    restfulierl_deps:ensure(),
    ensure_started(inets),
		ensure_started(xmerl),
		ok.

%% @spec stop() -> ok
%% @doc Stop the Restfulierl application and dependencies(take care!)
stop() ->
	Res = application:stop(restfulierl),
	application:stop(xmerl),
	application:stop(inets),
	Res.

%%
%% External API
%%

from_web(Uri) ->
	{ok, {{_HttpVersion, _StatusCode, _Message}, _Headers, Body}} = http:request(Uri),
	
	Resource = restfulierl_content_parser:parse(Uri, Body),
	ResourceName = Resource#resource.name,
	
	ResourceModule1 = smerl:new(ResourceName),
	{ok, ResourceModule2} = smerl:add_func(ResourceModule1,
														"test() -> "
														++ atom_to_list(ResourceName) ++ "_process ! {self(), state},"
														++ "receive Msg -> io:format(\"Msg = ~w~n\", [Msg]) end,"
														++ "test_ok."),
			
	ResourceModule3 = smerl:extend(restfulierl_resource, ResourceModule2),
	smerl:compile(ResourceModule3),
	
	register(list_to_atom(atom_to_list(ResourceName) ++ "_process"), spawn(ResourceName, loop, [Resource])),
	
	ResourceName.

%%
%% Internal API
%%

%% ensure that required application is started
ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.
