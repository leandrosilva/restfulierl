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

get_resource_test() ->
	restfulierl:start(),
	
	TestUri = "http://restfulie-test.heroku.com/orders/11.xml",
	
	Resource = restfulierl:get_resource(TestUri),

	?assertEqual(Resource#resource.uri, TestUri),
	
	Resource.