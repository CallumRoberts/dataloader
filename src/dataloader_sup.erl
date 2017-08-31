-module(dataloader_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor Callbacks
-export([init/1]).
 

%% Supervisor
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-spec start_link() -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Private  
init([]) ->
    MaxRestart = 5,
    MaxTime = 3600,
    SupFlags = #{ strategy =>  simple_one_for_one, intensity => MaxRestart, period => MaxTime},
    ChildSpecs = [#{id => dataloader,
		    start => {dataloader, start_link, []},
		    restart => temporary, 
		    shutdown => 5000}],
    {ok, {SupFlags, ChildSpecs}}.



