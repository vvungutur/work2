-module(super).

-behaviour(supervisor).

-export([start/0, start_shell/0, init/1]).
start() -> 
	supervisor:start_link({global, ?MODULE}, ?MODULE, []).

start_shell() -> 
	{_, ID} = supervisor:start_link({global, ?MODULE}, ?MODULE, []),
	unlink(ID).


init([]) ->



	RS = one_for_one,
	MR = 5,
	MTBR = 5,
	Flgs = {RS, MR, MTBR},

	Restart = permanent,
	Shutdown = infinity, 
	Type = worker,

	ChildSpec = {id, {server, start, []}, Restart, Shutdown, Type, [server]},

	{ok, {Flgs, [ChildSpec]}}.
