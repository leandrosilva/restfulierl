%%
%% Restfulierl, member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

%% @doc test module for Restfuilerl.

-module(restfulierl_test).
-author('Leandro Silva <leandrodoze@gmail.com>').

-export([from_web/0]).

from_web() ->
	restfulierl:start(),
	restfulierl:from_web("http://twitter.com/statuses/show/123.xml").