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
-export([get_resource/1, post_resource/1, post_resource/2, put_resource/1, delete_resource/1]).

-include("restfulierl.hrl").

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

get_resource(Uri) ->
	HttpResponse = http:request(Uri),
	
	_Resource = restfulierl_resource:from_http_response(Uri, HttpResponse).

post_resource(Resource) ->
	post_resource(Resource, Resource#resource.uri).

post_resource(_Resource, _Transition) when is_atom(_Transition) ->
	yet_not_implemented;

post_resource(_Resource, Uri) when is_list(Uri) ->
	Headers = [],
	ContentType = "application/xml",
	Body = "<order></order>",
	HttpOptions = [],
	Options = [{body_format, string}],
	
	_Response = http:request(post,
									{Uri, Headers, ContentType, Body}, HttpOptions, Options).

put_resource(_Resource) ->
	yet_not_implemented.

delete_resource(_Resource) ->
	yet_not_implemented.

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
