%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc marshal a resource to a xml.

-module(restfulierl_xml_marshaler).
-author('Leandro Silva <leandrodoze@gmail.com>').

%% external api
-export([resource_to_xml/2]).

-include("restfulierl.hrl").

-include_lib("xmerl/include/xmerl.hrl").

%%
%% External API
%%

%% marshal a resource record to a xml
resource_to_xml(_Uri, _Xml) ->
	ok.

%%
%% Internal APIs
%%
