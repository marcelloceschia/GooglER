-module(googleatom_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(?MODULE, []).

init(_Args) ->
	io:format("Starting: ~s: ~s~n", [?MODULE, _Args]),
	{ok, {{one_for_one, 1, 60}, [
		{blogger_srv, {blogger_srv, start_link, []},
		permanent, brutal_kill, worker, [blogger_srv]},
		{picasa_srv, {picasa_srv, start_link, []},
		permanent, brutal_kill, worker, [picasa_srv]}
	]}}.

