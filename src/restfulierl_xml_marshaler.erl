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
-export([to_xml/1]).

-include("restfulierl.hrl").

-include_lib("xmerl/include/xmerl.hrl").

%%
%% External API
%%

%% marshal a resource record to a xml
to_xml(ResourceState) ->
  _Xml = lists:flatten(xmerl:export_simple([ResourceState], xmerl_xml)).

%%
%% Internal APIs
%%
