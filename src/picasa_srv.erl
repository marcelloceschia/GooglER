-module(picasa_srv).
-behaviour(gen_server).

-include_lib("xmerl/include/xmerl.hrl").

-export([start/0, start/1, start_link/0, start_link/1, stop/1]).

-export([ 
	init/1, 
	handle_call/3,
	handle_cast/2,
	handle_info/2,
	terminate/2,
	code_change/3
	]).

-export([ 
	auth/1, auth/2, auth/3,
	new/3,
	new/4,
	new/5,
	reset/1,
	snapshot/1
	 ]).

-export([ tmpfile/1, 
	do_picasa/5 ]).

-record(state, {
		auth,
		credentials,
		requests,
		posts}).

%% supervision
start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

start(Auth) ->
	gen_server:start(?MODULE, [Auth], []).

start_link(Auth) ->
	gen_server:start_link(?MODULE, [Auth], []).

init([]) ->
	process_flag(trap_exit, true),
	{ok, #state { auth = [], requests = 0, posts = 0 } };
	
init([{_Username, _Password} = Auth]) ->
	process_flag(trap_exit, true),
	{ok, #state { auth = [], requests = 0, posts = 0, credentials = Auth } }.

snapshot(Srv) ->
	gen_server:call(Srv, snap).

stop(Srv) ->
	gen_server:call(Srv, stop).

% Using provided credentials at init time
auth(Srv) ->
	gen_server:call(Srv, auth).

auth(Srv, Username, Password) ->
	gen_server:call(Srv, {auth, Username, Password}).

auth(Username, Password) ->
	gen_server:call(?MODULE, {auth, Username, Password}).

% Posting to a default album, named "Blog"
new(User, Image, ImgName) ->
	gen_server:cast(?MODULE, {picasa, User, "Blog", Image, ImgName}).

% Posting to the specified Album
new(User, Album, Image, ImgName) ->
	gen_server:cast(?MODULE, {picasa, User, Album, Image, ImgName}).
	
new(Srv, User, Album, Image, ImgName) ->
	gen_server:cast(Srv, {picasa, User, Album, Image, ImgName}).

reset(Srv) ->
	gen_server:cast(Srv, reset).

%% gen_server callbacks
handle_call(snap, _Node, State) ->
	{reply, {ok, State}, State};

% Auth with picasa, ... with username and password
handle_call({auth, Username, Password}, _Node, State) ->
	Requests = State#state.requests,
	case google:auth(picasa, Username, Password) of
		{ok, Auth} ->
			{reply, {ok, Auth}, State#state{ 
					requests = Requests + 1, 
					auth = set_auth(picasa, Auth, State#state.auth),
					credentials = {Username, Password} } 
			};

		{error, Msg} ->
			{reply, {err, Msg}, State#state{ requests = Requests + 1 } } 
	end;

handle_call(auth, _Node, State) ->
	Requests = State#state.requests,
	case State#state.credentials of
		{Username, Password} ->
			case google:auth(picasa, Username, Password) of
				{ok, Auth} ->
					{reply, {ok, Auth}, State#state{ requests = Requests + 1, 
					auth = set_auth(picasa, Auth, State#state.auth) } };

				{error, Msg} ->
					{reply, {err, Msg}, State#state{ requests = Requests + 1 } } 
			end;

		_ ->
			{reply, {err, no_credentials}, State#state{ requests = Requests + 1} }
	end;

handle_call(stop, _Node, State) ->
	Reason = normal,
	{stop, Reason, Reason,State};
	% {stop, Reason, Reply, State}

handle_call(_Any, _Node, State) ->
	{reply, unknown, State}.

	
% Misc 
handle_info({'EXIT', _Pid, _Reason}, State) ->
	{noreply, State};

handle_info({picasa, Msg}, State) ->
	error_logger:info_msg("Picasa Image ~p~n", [Msg]),
	{noreplay, State};

handle_info(_Msg, State) ->
	io:format("OutofBand: ~p~n", [_Msg]),
	{noreply, State}.


% CASTs
handle_cast(reset, State) ->
	{User, Pass} = State#state.credentials,
	case google:auth(picasa, User, Pass) of
		{ok, Auth} ->
			NewState = #state{ posts = 0, requests = 0, auth=[{picasa, Auth}], credentials = {User, Pass} },
			{noreply, NewState};

		{error, _Msg} ->
			{noreply, State} 
	end;


handle_cast({picasa, User, Album, Image, ImgName}, State) ->
	Requests = State#state.requests,
	{ok, Auth, NewState} = get_auth(picasa, State),
	spawn_link(?MODULE, do_picasa, [Auth, User, Album, ImgName, Image]),
	{noreply, NewState#state{ requests = Requests + 1 }};

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, State, _Vsn) ->
	{ok, State}.

request_picasa(AuthToken, User, Album, ImgName, Data) ->
	Authorization = <<"GoogleLogin auth=", AuthToken/binary>>,
	Url = "http://picasaweb.google.com/data/feed/api/user/" ++ User ++ "/album/" ++ Album,
	case httpc:request(post,  					% Method ;)
			{ 
				Url, 					% URL
				[	{ "Authorization", binary_to_list(Authorization) }, % Headers
					{ "Slug", ImgName } 
				], 
				"image/jpeg",  				% Content-type
				Data	 				% Body
			},
		[ {timeout, 30000}, {sync, false} ],		% HTTPOptions
		[ {body_format, binary} ]) of 			% Options

		{ok, Result} -> 
			case Result of
				{{_, 201, _}, _Headers, Response} ->
					{ok, extract_urls(Response) };
					%{ok, Headers, Response};

				{_,_,Response} ->
					Response 
			end;
	
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason])
	end.

set_auth(Service, Token, Auth) ->
	List = lists:keydelete(Service, 1, Auth),
	[ {Service, Token} | List ].

get_auth(Type, State) ->
	case lists:keysearch(Type, 1, State#state.auth) of
		{value, {_, Auth}} ->
				{ok, Auth, State};
		false ->
			{User, Pass} = State#state.credentials,
			case google:auth(Type, User, Pass) of
				{ok, Auth} ->
					NewState = State#state{  auth = set_auth(Type, Auth, State#state.auth) },
					{ok, Auth, NewState};

				{error, _Msg} ->
					{error, _Msg, State} 
			end 
			% {User, Pass} = State#state.credentials,
			% gen_server:call(?MODULE, {auth, Type, User, Pass}) 
	end.
	
extract_urls(Data) ->
	{Xml, _} = xmerl_scan:string(binary_to_list(Data), []),
	[Url] = xmerl_xpath:string("/entry/media:group/media:content/@url", Xml),
	Thumbnails = xmerl_xpath:string("/entry/media:group/media:thumbnail/@url", Xml),
	{ extract_url(Url), [ extract_url(X) || X <- Thumbnails] }.
	
extract_url([]) ->
	"none";
extract_url(XmlAttribute) ->
	element(9, XmlAttribute).
	
get_file([$h,$t,$t,$p,$:,$/,$/ | _] = Url) ->
	TmpFile = tmpfile("/tmp"),
	case httpc:request(get, { Url, [] }, [ {timeout, 10000}, {sync, false} ], 
			[ {stream, TmpFile} ]) of 

		{ok, saved_to_file} ->
			get_file(TmpFile);
	
		_E ->
			_E
	end;
	
get_file(File) ->
	prim_file:read_file(File).

tmpfile(Path) ->
	tmpfile(Path, 0).

tmpfile(Path, N) ->
	{X,Y,Z} = erlang:now(),
	UniqueName = integer_to_list(X) ++ integer_to_list(Y) ++ 
			integer_to_list(Z) ++ integer_to_list(N),
	NewFile = lists:flatten([Path, "/", UniqueName, ".qterm"]),
	case filelib:is_file(NewFile) of
		true ->
			tmpfile(Path, N+1);
		false ->
			NewFile
	end.

do_picasa(Auth, User, Album, ImgName, Image) ->
	case get_file(Image) of
		{ok, File} ->
			case request_picasa(Auth, User, Album, ImgName, File ) of 
				{ok, Response} ->
					error_logger:info_msg("Picasa ~p~n", [Response]);

				Msg ->
					error_logger:info_msg("Picasa Reply ~p~n", [Msg]) 
			end;

		{error, Msg} ->
			?MODULE ! {error, Msg}
	end.
