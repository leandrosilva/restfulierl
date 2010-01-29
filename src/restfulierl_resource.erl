%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc a Restfuilerl resource.

-module(restfulierl_resource).
-author('Leandro Silva <leandrodoze@gmail.com>').

%% external api
-export([from_http_response/2]).

-include("restfulierl.hrl").

%%
%% External API
%%

from_http_response(Uri, HttpResponse) ->
	{ok, {{_HttpVersion, _StatusCode, _Message}, _Headers, Body}} = HttpResponse,
	
	{Xml, _Rest} = xmerl_scan:string(Body),
	
	_Resource = restfulierl_xml_unmarshaler:to_resource(Uri, Xml).