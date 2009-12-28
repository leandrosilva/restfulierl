%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc test module for Restfuilerl.

-module(restfulierl_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-include("restfulierl.hrl").

-include_lib("eunit/include/eunit.hrl").

from_web_test() ->
	restfulierl:start(),
	
	TestUri = "http://twitter.com/statuses/show/123.xml",
	
	Resource = restfulierl:from_web(TestUri),
	?assertEqual(Resource#resource.uri, TestUri).