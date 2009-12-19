%%
%% Restifulierl
%%

-module (restifulierl).
-author ('Leandro Silva (CodeZone) <leandrodoze@gmail.com>').

-export ([from_web/1]).

-record (resource, {url}).

from_web(Url) ->
	#resource{url = Url}.