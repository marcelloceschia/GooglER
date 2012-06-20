-module(google).
-export([ auth/3 ]).

build(Username, Password, Application, Service) ->
	Sep = get_sep(),
	Post = [ 
		<<"accountType=GOOGLE_OR_HOSTED">>, Sep,
		<<"Email=">>, Username, Sep, 
		<<"Passwd=">>,Password, Sep,
		<<"source=">>, Application, Sep,
		<<"service=">>, Service ],
	erlang:iolist_to_binary(Post).

% auth(Username, Password, Application) ->
% 	Sep = get_sep(),
% 	Post = [ 
% 		<<"accountType=HOSTED_OR_GOOGLE">>, Sep,
% 		<<"Email=">>, list_to_binary(Username), Sep, 
% 		<<"Passwd=">>, list_to_binary(Password), Sep,
% 		<<"source=">>, list_to_binary(Application), Sep,
% 		<<"service=blogger">> ],
% 	request(erlang:iolist_to_binary(Post)).

auth(blogger, Username, Password) ->
	Data = build(Username, Password, "googlER", "blogger"),
	request(Data);

auth(picasa, Username, Password) ->
	Data = build(Username, Password, "googlER", "lh2"),
	request(Data);

auth(content, Username, Password) ->
	Data = build(Username, Password, "googlER", "structuredcontent"),
	request(Data).

request(Data) ->
	case httpc:request(post,  
			{"https://www.google.com/accounts/ClientLogin", [], "application/x-www-form-urlencoded", Data},
			[ {timeout, 3000} ], 
			[{body_format, binary}]) 
		of 

		{ok, Result} -> 
			{_,_,Body} = Result,
			extract_auth(Body);
	
		{error, Reason} ->
			io:format("Error: ~p~n", [Reason]);
	
		_E ->
			io:format("Error: ~p~n", [_E])
	end.
			
get_sep() ->
	<<"&">>.

extract_auth(<<>>) ->
	{error, not_found};
extract_auth(<<"Error=", Rest/binary>>) ->
	Size = size(Rest) - 1,
	<<Msg:Size/binary, _/binary>> = Rest,
	{error, binary_to_list(Msg)};

extract_auth(<<"Auth=", Rest/binary>>) ->
	Size = size(Rest) - 1,
	<<Auth:Size/binary, _/binary>> = Rest,
	{ok, Auth};
extract_auth(<<_:1/binary, Rest/binary>>) ->
	extract_auth(Rest).
	

