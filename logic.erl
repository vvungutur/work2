-module(logic).

-export([start/0, stop/0, init/0, deposit/1, withdraw/1, balance/0]).



start() -> register(bank, spawn(logic, init, [])).

stop() -> bank ! {self(), stop}.

init() ->

	io:format("ok started bank~n", []),
	trans(0).

deposit(Val) -> bank ! {deposit, self(), Val}.

withdraw(Val) -> bank ! {withdraw, self(), Val}.

balance() -> bank ! {check, self()}.
 

trans(X) ->

	receive 

		{deposit, ID, Val} ->
			ID ! ok_deposit,			
			trans(X + Val);

		{withdraw, ID, Val} ->
			case Val =< X of		 
				true -> ID ! {ok_withdrawn, Val},
					trans(X - Val);
				false ->
					ID ! err_exceeds_balance,
					trans(X)
			end;
		{check, ID} -> 
			ID ! {balance_is, X},
			trans(X);
		{_, stop} ->
			done
	end.
