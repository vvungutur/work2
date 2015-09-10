-module(cclient).

-compile(export_all).

connect() ->

		{ok, Socket} = gen_tcp:connect({127,0,0,1}, 9000, []),
		send_loop(Socket, ID).


send_loop(Socket, ID) ->

	gen_tcp:send(Socket, {ID, MSG}),
	io:format("ID - ~s -> MSg - ~s~n", [ID, MSG]),

	case MSG of
		
		close -> 
			gen_tcp:send(Socket, {ID, "close"}),
			gen_tcp:close();

		_ -> 
			gen_tcp:send(Socket, {ID, MSG}),
			send_loop(Socket, ID)

	end.

	
