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
%% @doc Stop the Restfulierl application and dependencies (take care!).
stop() ->
	Res = application:stop(restfulierl),
	application:stop(xmerl),
	application:stop(inets),
	Res.

%%
%% External API
%%

%% @spec get_resource(Uri) -> Resource
%% @doc Get a resource from specified Uri.
get_resource(Uri) ->
	HttpResponse = http:request(Uri),
	
	_Resource = restfulierl_resource:from_http_response(Uri, HttpResponse).

%% @spec post_resource(Resource) -> Response
%% @doc Post a resource to Resource#resource.uri.
post_resource(Resource) ->
	post_resource(Resource, Resource#resource.uri).

%% @spec post_resource(Resource, Transition) -> Response
%% @doc Post a resource to specified Transition.
post_resource(_Resource, _Transition) when is_atom(_Transition) ->
	yet_not_implemented;

%% @spec post_resource(Resource, Uri) -> Response
%% @doc Post a resource to specified Uri.
post_resource(_Resource, Uri) when is_list(Uri) ->
	
	% XmlText = lists:flatten(xmerl:export_simple([_Resource], xmerl_xml)),
	% io:formart("XML: ~w", [XmlText]),
	
	Headers = [],
	ContentType = "application/xml",
	Body = "<order></order>", %restfulierl_xml_marshaler:to_xml(Resource),
	HttpOptions = [],
	Options = [{body_format, string}],
	
	_HttpResponse = http:request(post,
										{Uri, Headers, ContentType, Body}, HttpOptions, Options).

%% @spec put_resource(Resource) -> Response
%% @doc Put a resource to Resource#resource.uri.
put_resource(_Resource) ->
	yet_not_implemented.

%% @spec delete_resource(Resource) -> Response
%% @doc Delete a resource from Resource#resource.uri.
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
