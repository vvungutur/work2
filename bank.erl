-module(bank).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(bank, {id, balance}).

init_bank() ->

	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(bank, {[attributes, record_info(bank)]}),
	mneisa:close().

start_bank() ->
	mneisa:start(),
	mnesia:wait_for_tables([bank], 20000).

deposit(ID, AMT) ->
 
	case check_account_exits(ID) of
	
		{true, Bal} ->

			Write = fun() ->
			[Curent] = mnesia:read(bank, ID),
			Record = Current#bank{Current#bank.amt + AMT},
			mnesia:write(Record) end,
			mnesia:transaction(Write);

		false ->
		
			Record = #bank(id = ID, amt = AMT ),
			Write = fun() -> mnesia:write(Record) end,
			mnesia:transaction(Write)

	end.


withdraw(ID, AMT) ->
		
	case check_account_exists(ID) of

		{true, Bal} ->
				take(ID, AMT);
		false -> 
				id_not_found

	end.

check_account_exits(ID)  ->
			
		do(qlc:q([X#bank.balance || X <- mnesia:table(bank), X#bank.id =:= ID])).



do(Query) ->
	
		F = fun() -> qlc:e(Query) end,
		mnesia:transaction(F).


record_data() ->
	[

		{bank, 1, 100},
		{bank, 2, 223400},
		{bank, 3, 222100},
		{bank, 4, 698},
		{bank, 5, 72300},
		{bank, 6, 324200},
		{bank, 7, 1345},
		{bank, 8, 2545},
		{bank, 9, 8700},
		{bank, 10, 100},
			
		{name, 1, 'john smith'},		
		{name, 2, 'jack smith'},		
		{name, 3, 'jane smith'},		
		{name, 4, 'james smith'},			
		{name, 5, 'pam doe'},		
		{name, 6, 'steven doe'},		
		{name, 7, 'sam doe'},		
		{name, 8, 'sally doe'},		
		{name, 9, 'agatha doe'},
		{name, 10, 'vamsi vungutur'}		

	].

bulk_write() ->

	F = fun() -> lists:foreach(fun mnesia:write/1, record_date()) end,

		mnesia:transaction(F).
