%%
%% Restfulierl, a member of Restfulie initiative.
%%
%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.
%%
%% See more about Restfulie initiative on http://restfulie.caelum.com.br.
%%

{application, restfulierl,
 [{description, "Restfulierl, a member of Restfulie initiative"},
  {vsn,"0.0.1"},
  {modules, [
		restfulierl,
		restfulierl_deps
		]},
  {registered, []},
  {env, []},
  {applications, [kernel, stdlib, inets, xmerl]}]}.
