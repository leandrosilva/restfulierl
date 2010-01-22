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

%%
%% Setup
%%

restfulierl_test_() ->
	{setup,	fun setup/0, []}.

setup() ->
	restfulierl:start().

%%
%% Tests
%%

-define(RESOURCE_URI, "http://restfulie-test.heroku.com/orders/11.xml").

get_resource_test() ->	
	Resource = restfulierl:get_resource(?RESOURCE_URI),

	?assertEqual(Resource#resource.uri, ?RESOURCE_URI).

post_new_resource_test() ->
	Resource = #resource{
									uri = ?RESOURCE_URI,
									state = {order, [], []}},

	Response = restfulierl:post_resource(Resource),
	
	?assertMatch({ok, _}, Response).
	
post_new_resource_to_uri_test() ->
	Resource = #resource{state = {order, [], []}},

	Response = restfulierl:post_resource(Resource),
	
	?assertMatch(yet_not_implemented, Response).