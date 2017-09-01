-module(dlel_sup).
-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor Callbacks
-export([init/1]).


%% Supervisor
-type start_link_err() :: {already_started, pid()} | shutdown | term().
-type start_link_ret() :: {ok, pid()} | ignore | {error, start_link_err()}.

-spec start_link(atom()) -> start_link_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% Private  
init([]) ->
    MaxRestart = 1,
    MaxTime = 3600,
    SupFlags = #{ strategy => one_for_one, intensity => MaxRestart, period => MaxTime}, 
    ChildSpecs = [#{id => ets_cache,
		    start => {ets_cache, start_link, []},
		    restart => permanent,
		    shutdown => 5000,
		    type => worker,
		    modules => [ets_cache]},
		  
		  #{id => dataloader_sup,
		    start => {dataloader_sup, start_link, []}, 
		    restart => permanent,
		    shutdown => infinity,
		    type => supervisor,
		    modules => [dataloader_sup]}],
    {ok, {SupFlags, ChildSpecs}}. 







