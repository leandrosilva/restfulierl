%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc a Restfuilerl resource parent module.

-module(restfulierl_resource).
-author('Leandro Silva <leandrodoze@gmail.com>').

%% external api
-export([loop/1]).

-include("restfulierl.hrl").

%%
%% External API
%%

loop(ResourceState) ->
	receive
		{SenderPid, state} ->
			SenderPid ! ResourceState;
		{SenderPid, _} ->
			SenderPid ! {error, unknow_message}
	end,
	loop(ResourceState).