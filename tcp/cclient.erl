-module(cclient).

-compile(export_all).

-import(cserver, [auth/2]).

authenticat() ->

	UserName = io:get_line("enter user name"),
	Password = io:get_line("enter password"),
	Auth = cserver:auth(UserName, Password),
	case Auth of

		false ->
			authenticate();

		_ ->
			{_, ID} = Auth,
			connect(ID) 
	
	end.
connect(ID) ->
		
	{ok, Socket} = gen_tcp:connect({127,0,0,1}, 9000, []),
	send_loop(Socket, {auth, ID}).


		


send_loop(Socket, Info) ->

	{_, ID} = Info,

	UserName = cserver:get_username(ID),

	MSG = io:get_line("~s ::: ", [UserName]),

	gen_tcp:send(Socket, {ID, MSG}),

	case MSG of
		
		close -> 
			gen_tcp:send(Socket, {ID, "close"}),
			gen_tcp:close();

		_ -> 
			gen_tcp:send(Socket, {ID, MSG}),
			send_loop(Socket, ID)

	end.


