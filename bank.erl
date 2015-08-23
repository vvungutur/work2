-module(bank).
-compile(export_all).

-include_lib("stdlib/include/qlc.hrl").

-record(bank, {id, balance}).
-record(person, {id, name, age, credit}).

init_bank() ->

	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(bank, [{attributes, record_info(fields, bank)}]),
	mnesia:create_table(person, [{attributes, record_info(fields, person)}]),
	mnesia:stop().

start_bank() ->
	mnesia:start(),
	mnesia:wait_for_tables([bank, person], 2000),
	bulk_write().

close_bank() ->
		mnesia:stop().


deposit(ID, AMT) ->
 
	case check_account_exists(ID) of
	
		{true, Account} ->

			{ok, ReturnBalance, OriginalBalance} = give(Account, AMT),
			{true, ReturnBalance, OriginalBalance};

		false ->
		
			AddRecord = #bank{id=ID, balance=AMT},
			Write = fun() -> mnesia:write(AddRecord) end,
			mnesia:transaction(Write),
			ReturnBalance = getBalance(ID),
			{true, ReturnBalance, 0}
			

	end.

withdraw(ID, AMT) ->
		
	case check_account_exists(ID) of

		{true, Account} ->
			{Status, Val1, Val2} = take(Account, AMT),
				case Status of
		 
					ok ->
						ReturnBalance = Val1,
						OriginalBalance = Val2,
						{true, ReturnBalance, OriginalBalance};

					nef -> 
						AMT_Request = Val1,
						Balance = Val2,
						{false, AMT_Request, Balance}
				end;

		false -> 
			id_not_found

	end.

getBalance(ID) ->
	
		case check_account_exists(ID) of

			{true, Account} ->
					Account#bank.balance;					

			false -> 
				id_not_found

		end.
		
transfer(SID, REID, S_AMT) ->

	case withdraw(SID, S_AMT) of 
		
		{true, ReturnBalanceW, _ } ->
						{_, ReturnBalanceD, OriginalBalance} = deposit(REID, S_AMT),
						io:format("~p just sent ~p, now has ~p, ~p had ~p, now has ~p, thank you. ~n", 
							[SID, S_AMT, ReturnBalanceW, REID, OriginalBalance, ReturnBalanceD]);

		{false, AMT, Balance} ->
				io:format("(~p) not enough funds (~p) to implement transfer of ~p ~n", [SID, Balance, AMT]);
		

		id_not_found ->	
				io:format("Withdrawal account not found exiting operation, goodbye ~n", [])
		end. 


check_account_exists(ID)  ->
			
		F = fun() ->[Account] = mnesia:read(bank, ID), Account end,
		Record = mnesia:transaction(F),
		{Status, Value} = Record, 

		case Status of 
			
			atomic ->
				{true, Value};

			aborted ->
				false

		end.


take(Account, AMT) ->

	Balance = Account#bank.balance,
	ID = Account#bank.id,

	case Balance >= AMT of
	
		
		true ->
			updateBalance(sub, Account, AMT),
			ReturnBalance =  getBalance(ID),
			{ok, ReturnBalance, Balance};

		false ->
			{nef, AMT, Balance}

	end.

give(Account, AMT) ->
			
		Balance = Account#bank.balance,		
		ID = Account#bank.id,		
		updateBalance(add, Account, AMT),
		ReturnBalance = getBalance(ID),
		{ok, ReturnBalance, Balance}.


updateBalance(Action, Account, Amt) ->

	case Action of 

		add -> 	
			UpdateAmount = Account#bank.balance + Amt;

		sub ->
			UpdateAmount = Account#bank.balance - Amt


	end,

	
	Write = fun() -> UpdateRecord = Account#bank{balance=UpdateAmount},
		 mnesia:write(UpdateRecord) end,
		 mnesia:transaction(Write).
	



do(Query) ->
	F = fun() -> qlc:e(Query) end,
		mnesia:transaction(F).


getRecords() ->

	do(qlc:q([ {X#person.id, X#person.name, Y#bank.balance, X#person.age, X#person.credit} 
		|| X <- mnesia:table(person), Y <- mnesia:table(bank), X#person.id =:= Y#bank.id])).	

getRecords(ID) ->

	do(qlc:q([ {X#person.id, X#person.name, Y#bank.balance, X#person.age, X#person.credit} 
		|| X <- mnesia:table(person), Y <- mnesia:table(bank), X#person.id =:= Y#bank.id, X#person.id =:= ID])).	


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
			
		{person, 1, 'john smith', 25 , 700},		
		{person, 2, 'jack smith', 42, 700},		
		{person, 3, 'jane smith', 55, 800},		
		{person, 4, 'james smith', 21, 600},			
		{person, 5, 'pam doe', 33, 700},		
		{person, 6, 'steven doe', 34, 800},		
		{person, 7, 'sam doe', 22, 467},		
		{person, 8, 'sally doe', 30, 650},		
		{person, 9, 'agatha doe', 40, 750},
		{person, 10, 'vamsi vungutur', 45, 800}		

	].

bulk_write() ->

	F = fun() -> lists:foreach(fun mnesia:write/1, record_data()) end,

		mnesia:transaction(F).
