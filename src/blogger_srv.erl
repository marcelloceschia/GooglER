-module(blogger_srv).
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
	auth/1, auth/2, auth/3, auth/4,
	new/5, new/6,
	reset/1,
	snapshot/1
	 ]).

-export([ entry_new/4, do_blogger/5 ]).

-record(state, {
		auth,
		credentials,
		requests,
		posts}).

%% supervision

start() ->
	gen_server:start(?MODULE, [], []).

start_link() ->
	gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

start(Auth) ->
	%gen_server:start({local, ?MODULE}, ?MODULE, [Auth], []).
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

%% commands

% Using provided credentials at init time
auth(Srv) ->
	gen_server:call(Srv, auth).

auth(Username, Password) ->
	gen_server:call(?MODULE, {auth, Username, Password}).

auth(Srv, Username, Password) ->
	gen_server:call(Srv, {auth, Username, Password}).

auth(Srv, Service, Username, Password) ->
	gen_server:call(Srv, {auth, Service, Username, Password}).

new(Srv, BlogId, Title, Tags, Content) ->
	gen_server:cast(Srv, {blogger, BlogId, Title, Tags, Content}).
	
new(Srv, BlogId, Title, Tags, Content, {AuthorName, AuthorEmail} ) ->
	gen_server:cast(Srv, {post, BlogId, Title, Tags, Content, AuthorName, AuthorEmail}).

reset(Srv) ->
	gen_server:cast(Srv, reset).

% gen_server calls
% handle_call({post, BlogId, Title, Tags, Content}, _Node, State) ->
% 	Requests = State#state.requests,
% 	Auth = State#state.auth,
% 	Data = entry_new(Title, {"n/a", "n/a"}, Content, Tags),
% 	case request(Auth, BlogId, Data) of 
% 		{ok, PostId} ->
% 			{reply, {ok, PostId}, State#state{ requests = Requests + 1 } };
% 
% 		Msg ->
% 			{reply, {err, Msg}, State#state{ requests = Requests + 1 } }
% 	end;

	
handle_call({post, BlogId, Title, Tags, Content, AuthorName, AuthorEmail}, _Node, State) ->
	Requests = State#state.requests,
	Auth = State#state.auth,
	Data = entry_new(Title, {AuthorName, AuthorEmail}, Content, Tags),
	case request(Auth, BlogId, Data) of 
		{ok, PostId} ->
			{reply, {ok, PostId}, State#state{ requests = Requests + 1 } };

		Msg ->
			{reply, {err, Msg}, State#state{ requests = Requests + 1 } }
	end;

handle_call(snap, _Node, State) ->
	{reply, {ok, State}, State};

% Auth against blogger, picasa, ... with username and password
handle_call({auth, Service, Username, Password}, _Node, State) ->
	Requests = State#state.requests,
	case google:auth(Service, Username, Password) of
		{ok, Auth} ->
			{reply, {ok, Auth}, State#state{ 
					requests = Requests + 1, 
					auth = set_auth(Service, Auth, State#state.auth),
					credentials = {Username, Password} } 
			};

		{error, Msg} ->
			{reply, {err, Msg}, State#state{ requests = Requests + 1 } } 
	end;

% Auth against blogger with username and password
handle_call({auth, Username, Password}, _Node, State) ->
	Requests = State#state.requests,
	case google:auth(blogger, Username, Password) of
		{ok, Auth} ->
			{reply, {ok, Auth}, State#state{ 
					requests = Requests + 1, 
					auth = set_auth(blogger, Auth, State#state.auth),
					credentials = {Username, Password} } 
			};

		{error, Msg} ->
			{reply, {err, Msg}, State#state{ requests = Requests + 1 } } 
	end;

handle_call(auth, _Node, State) ->
	Requests = State#state.requests,
	case State#state.credentials of
		{Username, Password} ->
			case google:auth(blogger, Username, Password) of
				{ok, Auth} ->
					{reply, {ok, Auth}, State#state{ requests = Requests + 1, 
					auth = set_auth(blogger, Auth, State#state.auth) } };

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

handle_info({blogger, Msg}, State) ->
	error_logger:info_msg("Blogger Post ~p~n", [Msg]),
	{noreply, State};

handle_info(_Msg, State) ->
	io:format("OutofBand: ~p~n", [_Msg]),
	{noreply, State}.


handle_cast(reset, State) ->
	{User, Pass} = State#state.credentials,
	case google:auth(blogger, User, Pass) of
		{ok, Auth} ->
			NewState = #state{ posts = 0, requests = 0, auth=[{blogger, Auth}], credentials = {User, Pass} },
			{noreply, NewState};

		{error, _Msg} ->
			{noreply, State} 
	end;


handle_cast({blogger, BlogId, Title, Tags, Content}, State) ->
	Requests = State#state.requests,
	{ok, Auth, NewState} = get_auth(blogger, State),
	spawn_link(?MODULE, do_blogger, [Auth, BlogId, Title, Tags, Content]),
	{noreply, NewState#state{ requests = Requests + 1 }};
	

handle_cast(_Msg, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_, State, _Vsn) ->
	{ok, State}.

% Internal stuff	
request(AuthToken, BlogId, Data) ->
	Body = iolist_to_binary(Data), 
	%io:format("Sending: ~nContent-length: ~p~nBody:~n~s~n", [ size(Body), Body ]),
	Authorization = <<"GoogleLogin auth=", AuthToken/binary>>,
	Url = "http://www.blogger.com/feeds/" ++ BlogId ++ "/posts/default", 
	case httpc:request(post,  					% Method ;)
			{ 
				Url, 					% URL
				[	{ "Authorization", binary_to_list(Authorization) } ], % Headers

				"application/atom+xml; charset=utf-8",  % Content-type
				Body	 				% Body
			},
		[ {timeout, 5000}, {sync, false} ],		% HTTPOptions
		[ {body_format, binary} ]) of 			% Options

		{ok, Result} -> 
			case Result of
				{{_, 201, _}, Headers, _Response} ->
					PostId = get_postid(Headers),
					{ok, PostId};

				{_,_,Response} ->
					Response 
			end;
	
		{error, Reason} ->
			error_logger:error_msg("Error: ~p~n", [Reason])
	end.


%% Creating ATOM post
entry_new(Title, Author, { Content, ContentType }, Tags) when is_list(Tags) ->
	NTitle = post_title(Title),
	NContent = post_content(Content, ContentType),
	NTags = post_tags(Tags),
	NAuthor = post_author(Author),
	[ 
		entry_header(), 
		NTitle, 
		NContent,
		NAuthor,
		NTags,
		entry_footer()
	];

entry_new(Title, Author, Content, Tags) ->
	entry_new(Title, Author, {Content, text}, Tags).

entry_header() ->
	<<"<entry xmlns='http://www.w3.org/2005/Atom'>">>.

entry_footer() ->
	<<"</entry>">>.
	
post_title(Title) ->
	[ <<"<title type='text'>">>, iolist_to_binary(Title), <<"</title>">> ].

post_author({ AuthorName, AuthorEmail }) ->
	[ 
	<<"<author><name>">>, iolist_to_binary(AuthorName), <<"</name><email>">>,
	iolist_to_binary(AuthorEmail), <<"</email></author>">> ];
post_author(AuthorEmail) ->
	post_author({ "", AuthorEmail }).

post_content(Content, html) ->
	post_content(Content, "html");
post_content(Content, xhtml) ->
	post_content(Content, "xhtml");
post_content(Content, text) ->
	post_content(Content, "text");
post_content(Content, ContentType) ->
	[ <<"<content type='">>, iolist_to_binary(ContentType), <<"'>">>, 
	Content, 
	<<"</content>">> ].

post_tags([]) ->
	<<>>;	
post_tags(List) ->
	lists:map(
		fun(X) -> 
			[ <<"<category scheme='http://www.blogger.com/atom/ns#' term='">>, 
			iolist_to_binary(X) , 
			<<"'/>">> ] 
		end, 
		List).

get_postid([]) ->
	"none";
get_postid(Headers) ->
	case lists:keysearch("location", 1, Headers) of
		{value, {_, Value}} ->
			lists:last( string:tokens(Value, "/") );

		_ ->
			"none"
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
	
do_blogger(Auth, BlogId, Title, Tags, Content) ->
	Data = entry_new(Title, {"n/a", "n/a"}, Content, Tags),
	case request(Auth, BlogId, Data) of 
		{ok, PostId} ->
			error_logger:info_msg("Blogger Post ~p~n", [PostId]);

		Msg ->
			error_logger:info_msg("Blogger Reply ~p~n", [Msg]) 
	end.
