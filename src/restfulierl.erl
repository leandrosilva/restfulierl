%%
%% Restfulierl, member of Restfulie initiative.
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

from_web(Url) ->
	{ok, {{_HttpVersion, _StatusCode, _Message}, _Headers, Body}} = http:request(Url),
	new_resource(Url, Body).

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

%% create new resource record from response's body
new_resource(Url, Body) ->
	{Xml, _Rest} = xmerl_scan:string(Body),
	
	RootElement = parse_xml_element(Xml),
	{Name, Attributes, Children} = RootElement,
	
	_Resource = #resource{url = Url, state = {Name, Attributes, Children}, next_states = [yet_not_implemented]}.

%% parse single element
parse_xml_element(Xml) ->
	{xmlElement, Name, _, _, _, _Parents, _Position, Attributes, Content, _, _, _} = Xml,
	_ParsedElement = {Name, parse_xml_attributes(Attributes), parse_xml_children(Content)}.

%% parse the element's attributes
parse_xml_attributes(Attributes) ->
	parse_xml_attributes(Attributes, []).

parse_xml_attributes([HeadAttribute | TailAttributes], ParsedAttributes) ->
	{xmlAttribute, Name, _, _, _, _, _, _, Value, _} = HeadAttribute,
	ParsedAttribute = {attribute, Name, Value},
	parse_xml_attributes(TailAttributes, [ParsedAttribute | ParsedAttributes]);

parse_xml_attributes([], ParsedAttributes) ->
	lists:reverse(ParsedAttributes).
	
%% parse the element's childen
parse_xml_children(Elements) ->
	parse_xml_children(Elements, []).

parse_xml_children([{xmlText, _, _, _, Value, text} | TailElements], ParsedElements) ->
	case TailElements of
		[{xmlElement, _, _, _, _, _, _, _, _, _, _, _} | _] ->
			parse_xml_children(TailElements, ParsedElements);
		_ ->
			parse_xml_children(TailElements, [Value | ParsedElements])
	end;
		
parse_xml_children([HeadElement | TailElements], ParsedElements) ->
	ParsedElement = parse_xml_element(HeadElement),
	parse_xml_children(TailElements, [ParsedElement | ParsedElements]);

parse_xml_children([], ParsedElements) ->
	lists:reverse(ParsedElements).
