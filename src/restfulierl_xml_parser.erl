%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc parse for xml mime type.

-module(restfulierl_xml_parser).
-author('Leandro Silva <leandrodoze@gmail.com>').

%% external api
-export([xml_to_resource/2]).

-include("restfulierl.hrl").

-include_lib("xmerl/include/xmerl.hrl").

%%
%% External API
%%

%% parse a xml (from response's body) to a resource record
xml_to_resource(Uri, Xml) ->
	RootElement = parse_xml_element(Xml),
	{Name, Attributes, Children} = RootElement,
	
	_Resource = #resource{
									uri = Uri,
									state = {Name, Attributes, Children},
									transitions = [
										#transition{
												name = "google",
												uri = "http://www.google.com"},
										#transition{
												name = "yahoo",
												uri = "http://www.yahoo.com"}]}.

%%
%% Internal APIs
%%

%% parse single element
parse_xml_element(Xml) ->
	{xmlElement, Name, _, _, _, _Parents, _Position, Attributes, Content, _, _, _} = Xml,
	
	case Name of
		'atom:link' ->
			{Rel, Href} = parse_xml_atom_link_attributes(Attributes),
			
			_ParsedTransition = #transition{name = Rel, uri = Href};
		_ ->
			_ParsedElement = {Name, parse_xml_attributes(Attributes), parse_xml_children(Content)}
	end.
	
%% parse the element's attributes
parse_xml_attributes(Attributes) ->
	parse_xml_attributes(Attributes, []).

parse_xml_attributes([HeadAttribute | TailAttributes], ParsedAttributes) ->
	{xmlAttribute, Name, _, _, _, _, _, _, Value, _} = HeadAttribute,
	ParsedAttribute = {attribute, Name, Value},
	parse_xml_attributes(TailAttributes, [ParsedAttribute | ParsedAttributes]);

parse_xml_attributes([], ParsedAttributes) ->
	lists:reverse(ParsedAttributes).
	
%% parse the atom:link as transition
parse_xml_atom_link_attributes(Attributes) ->
	ParsedAttributes = parse_xml_attributes(Attributes),

	[{attribute, rel, Rel} | NextAttributes] = ParsedAttributes,
	[{attribute, href, Href} | _] = NextAttributes,

	{Rel, Href}.
		
%% parse the element's childen
parse_xml_children(Elements) ->
	parse_xml_children(Elements, []).

parse_xml_children([{xmlText, _, _, _, Value, text} | TailElements], ParsedElements) ->
	case TailElements of
		[{xmlElement, _, _, _, _, _, _, _, _, _, _, _} | _] ->
			parse_xml_children(TailElements, ParsedElements);
		[] ->
			parse_xml_children(TailElements, ParsedElements);
		_ ->
			parse_xml_children(TailElements, [Value | ParsedElements])
	end;
		
parse_xml_children([HeadElement | TailElements], ParsedElements) ->
	ParsedElement = parse_xml_element(HeadElement),
	parse_xml_children(TailElements, [ParsedElement | ParsedElements]);

parse_xml_children([], ParsedElements) ->
	lists:reverse(ParsedElements).
