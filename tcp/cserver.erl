-module(cserver).

-compile(export_all).

-record(user, {id, name, username, password}).
-record(log, {chatid, id, message, ts}).

-include_lib("stdlib/include/qlc.hrl").

init_store() ->

		mnesia:create_schema([node()]),
		mnesia:start(),
		mnesia:create_table(user, [{attributes, record_info(fields, user)}]),
		mnesia:create_table(log, [{attributes, record_info(fields, log)}]),
		mnesia:stop().


start() ->

	mnesia:start(),
	mnesia:wait_for_tables([user, message], 200),
	bulk_write().


bulk_write() ->

	F = fun() -> lists:foreach(fun mnesia:write/1, data()) end,
	mnesia:transaction(F).


data() ->

	[
		{user, 1, "vamsi vungutur", "vv", "erlang12"},
		{user, 2, "rahul vungutur", "rv", "clojur12"}

	].
	

write_log(ChatID, ID, Message) ->

	F = fun() -> mensia:write(#log{chatid=ChatID, id=ID, message=Message, ts=erlang:now()}) end,
	mnesia:transaction(F).

get_conv(ChatID) ->

	execute(qlc:q([{Y#user.name, X#log.message} || X <- meneisa:table(log), Y <- mnesia:table(user), 
	X#log.id =:= ChatID, X#log.id =:= Y#user.id])).

get_user_utterences(UserID) ->

	execute(qlc:q([X#log.message || X <- mneisa:table(log), X#log.id =:= UserID])).

execute(Query) ->

	F = fun() -> qlc:e(Query) end,
	mnesia:transaction(F).



get_pk_id() ->

		Z = execute(qlc:q([X#log.chatid || X <- mnesia:table(log)])),

		case Z of
			

			[] -> 1;

			_ -> lists:max(Z) + 1

		end.

authenticate(UN, PW) ->
			[Z] = execute(qlc:q([{X#user.id, X#user.name, X#user.username} 
			|| X <- mnesia:table(user), X#user.username =:= UN, X# user.password =:= PW])),
			Z.
