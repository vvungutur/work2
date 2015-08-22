-module(bank).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(bank, {id, balance}).

init_bank() ->

	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(bank, {[attributes, record_info(fields, bank)]}),
	mneisa:close().

start_bank() ->
	mneisa:start(),
	mnesia:wait_for_tables([bank], 20000).

close_bank() ->
		mnesia:close().


deposit(ID, AMT) ->
 
	case check_account_exists(ID) of
	
		{true, Account} ->

			{ok, AMT, ReturnBalance} = give(Account, AMT),
			{true, ID, ReturnBalnce};

		_ ->
		
			AddRecord = #bank{id=ID, balance=AMT},
			Write = fun() -> mnesia:write(AddRecord) end,
			mnesia:transaction(Write)

	end.

withdraw(ID, AMT) ->
		
	case check_account_exists(ID) of

		{true, Account} ->
			{OK, AMT, Balance, ReturnBalance} = take(Account, AMT),
			{true, AMT, Balance, ReturnBalance};

		_ -> 
				id_not_found

	end.

transfer(SID, REID, S_AMT) ->

	case Withdraw(SID, AMT) of 
		
		{true, S_AMT, Balance, ReturnBalance} ->
						deposit(REID, AMT),
						io:format("~p just sent ~p, now has ~p, ~p had ~p, now has ~p, thank you.", [SID, ReturnBalance, REID, ]);

		_ -> error



check_account_exists(ID)  ->
			
			[Account] = mnesia:read(bank, ID),
			{true, Account}.


do(Query) ->
	
		F = fun() -> qlc:e(Query) end,
		mnesia:transaction(F).



take(Account, AMT) ->

	Balance = Account#bank.balance,

	case Balance > AMT of
	
		
		true ->
			updateBalance(sub, Account, AMT),
			ReturnBalance = Account#bank.balance,
			{ok, AMT, Balance, ReturnBalance};

		false ->
			{nef, Balance}

	end.

give(Account, AMT) ->	
		
		updateBalance(add, Account, AMT),
		ReturnBalance = Account#bank.balance,
		{ok, AMT, ReturnBalance}.





updateBalance(Action, Account, Amt) ->

	case Action of 

		add -> 	
			UpdateAmount = Account#bank.balance + Amt;

		sub ->
			UpdateAmount = Account#bank.balance - Amt


	end,

	
	Write = fun() -> UpdateRecord = Account#bank{balance=UpdateAmount},
		 mnesia:write(UpdateRecord) end,
		 mnesia:transaction(Write)
	




		

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

	F = fun() -> lists:foreach(fun mnesia:write/1, record_data()) end,

		mnesia:transaction(F).
