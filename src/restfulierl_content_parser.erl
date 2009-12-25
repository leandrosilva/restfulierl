%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc parse for differents mime types.

-module(restfulierl_content_parser).
-author('Leandro Silva <leandrodoze@gmail.com>').

%% external api
-export([parse/2]).

-include("restfulierl.hrl").
-include_lib("xmerl/include/xmerl.hrl").

%%
%% External API
%%

%% parse a response's body to a resource record
parse(Uri, Body) ->
	{Xml, _Rest} = xmerl_scan:string(Body),
	
	_Resource = restfulierl_xml_content_parser:parse(Uri, Xml).