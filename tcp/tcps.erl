-module(tcps).

-compile(export_all).

-import(cstore, [get_pk_id/0, write_log/3]).

init_mnesia() ->
	cstore:init_schema().

start() ->
	
	{ok, Listen} = gen_tcp:listen(9000, [binary, {active, true}]),
	{ok, Accept} = gen_tcp:accept(Listen),
	loop(Accept, 0).



loop(Accept, ChatID) ->

	case ChatID of

		0 ->
			Cid = cstore:get_pk_id();

		_ ->	Cid = ChatID	

	end,

	receive
        	{tcp, Socket, <<"quit", _/binary>>} ->
        		gen_tcp:close(Socket);

        	{tcp, Socket, Msg} ->
				io:format("~s~n",[Msg]),
				cstore:write_log(Cid, Socket, Msg),	   
			        %gen_tcp:send(Socket, Msg),
            			loop(Socket, ChatID)
    	end.

