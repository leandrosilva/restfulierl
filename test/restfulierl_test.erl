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
%% Describing Restfulierl
%%

describe_restfulierl_test_() ->
	[fun before_all/0,
	  fun should_get_a_resource_from_a_valid_uri/0,
	  fun should_post_a_new_resource_to_its_uri/0,
	  fun should_post_a_new_resource_to_a_valid_uri/0,
	  fun should_post_resource_to_one_of_its_transitions/0].

%%
%% Setup
%%

-define(RESOURCE_URI, "http://restfulie-test.heroku.com/orders/11.xml").

before_all() ->
	restfulierl:start().

%%
%% Scenary
%%

should_get_a_resource_from_a_valid_uri() ->
  io:format(user, "should_get_a_resource_from_a_valid_uri~n", []),
  
	Resource = restfulierl:get_resource(?RESOURCE_URI),
	
	% {resource,"http://restfulie-test.heroku.com/orders/11.xml",
	%       {order,[],
	%           [{'created-at',[],["2010-01-01T05:15:45Z"]},
	%            {'customer-name',[],["Leandro Silva"]},
	%            {id,[],["11"]},
	%            {status,[],["cancelled"]},
	%            {'updated-at',[],["2010-02-03T14:07:18Z"]},
	%            "\n"]},
	%       [{transition,latest,
	%            "http://restfulie-test.heroku.com/orders/11"}]}}

	% matching uri
	?assertEqual(Resource#resource.uri, ?RESOURCE_URI),
	
	% matching state
	OrderState = Resource#resource.state,
	
	{order, [], OrderAttributes} = OrderState,
	
	[CreatedAt | NextOrderAttributes1] = OrderAttributes,
	?assertMatch({'created-at',[],["2010-01-01T05:15:45Z"]}, CreatedAt),
	
	[CustomerName | NextOrderAttributes2] = NextOrderAttributes1,
	?assertMatch({'customer-name',[],["Leandro Silva"]}, CustomerName),

	[Id | NextOrderAttributes3] = NextOrderAttributes2,
	?assertMatch({id,[],["11"]}, Id),

	[Status | NextOrderAttributes4] = NextOrderAttributes3,
	?assertMatch({status,[],["cancelled"]}, Status),

	[UpdateAt | _] = NextOrderAttributes4,
	?assertMatch({'updated-at',[],["2010-02-03T14:07:18Z"]}, UpdateAt),
	
	% matching transitions
	OrderTransitions = Resource#resource.transitions,
	
	[Latest | _NextOrderTransitions] = OrderTransitions,
	?assertMatch({transition, latest, "http://restfulie-test.heroku.com/orders/11"}, Latest).

should_post_a_new_resource_to_its_uri() ->
  io:format(user, "should_post_a_new_resource_to_its_uri~n", []),
  
	Resource = #resource{
									uri = ?RESOURCE_URI,
									state = {order, [], []}},

	Response = restfulierl:post_resource(Resource),
	
	?assertMatch({ok, _}, Response).

should_post_a_new_resource_to_a_valid_uri() ->
  io:format(user, "should_post_a_new_resource_to_a_valid_uri~n", []),

	Resource = #resource{state = {order, [], []}},

	Response = restfulierl:post_resource(Resource, ?RESOURCE_URI),

	?assertMatch({ok, _}, Response).
	
should_post_resource_to_one_of_its_transitions() ->
  io:format(user, "should_post_resource_to_one_of_its_transitions~n", []),
  
	Resource = #resource{state = {order, [], []}},

	Response = restfulierl:post_resource(Resource, pay),

	?assertMatch(yet_not_implemented, Response).
  