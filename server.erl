-module(server).
-compile(export_all).
-behaviour(gen_server).

start() -> gen_server:start_link({global,?MODULE},?MODULE, [], []).

stop() -> done(), gen_server:call({global, ?MODULE}, stop).

init([]) ->
	io:format("server started~n", []),
	bank:start_bank(),
	{ok, []}.

deposit(ID, AMT) -> gen_server:call({global, ?MODULE}, {deposit, ID, AMT}).
withdraw(ID, AMT) -> gen_server:call({global, ?MODULE}, {withdraw, ID, AMT}).
transfer(SID, REID, AMT) -> gen_server:call({global, ?MODULE}, {transfer, SID, REID, AMT}).
balance(ID) -> gen_server:call({global, ?MODULE}, {balance, ID}).
all_records() -> gen_server:call({global, ?MODULE}, all_records).
record(ID) -> gen_server:call({global, ?MODULE}, {record, ID}).
done() -> gen_server:call({global, ?MODULE}, done).


handle_call(stop, From, State) -> {stop, normal, stopped, State};
handle_call({deposit, ID, AMT} , From, State) -> Result = bank:deposit(ID, AMT), {reply, Result, State};
handle_call({withdraw, ID, AMT} , From, State) -> Result = bank:withdraw(ID, AMT), {reply, Result, State};
handle_call({transfer,SID, REID,  AMT} , From, State) -> Result = bank:transfer(SID, REID, AMT), {reply, Result, State};
handle_call({balance, ID}, From, State) -> Result = bank:getBalance(ID), {reply, Result, State}; 
handle_call(all_records, From, State) -> Result = bank:getRecords(), {reply, Result, State}; 
handle_call({record, ID}, From, State) -> Result = bank:getRecords(ID), {reply, Result, State}; 
handle_call(done, From, State) -> bank:close_bank(), {reply, closed, State}.


handle_cast(Msg, State) -> {noreply, State}.

handle_info(Info, State) -> {noreply, State}.

terminate(Reason, State) -> ok.

code_change(Old_Vsn, State, Extra) -> ok.
		

