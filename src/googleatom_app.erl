-module(googleatom_app).
-author('rolphin@shootit.fr').

-behaviour(application).
-export([start/2,stop/1]).

%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for googleatom_app.
start(_Type, _StartArgs) ->
	application:start(inets),
	application:start(crypto),
	crypto:start(),
	application:start(public_key),
	application:start(ssl),
	io:format("Starting: ~s: ~s,~s~n", [?MODULE, _Type, _StartArgs]),
	googleatom_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for mochiweb.
stop(_State) ->
    ok.
