-module(ets_cache).
-behaviour(gen_server).

-export([start_link/0, clear_id/2, clear_all/1, get_val/2, search_val/2]).

-export([init/1, 
	 handle_call/3, 
	 handle_cast/2, 
	 handle_info/2,
	 terminate/2, 
	 code_change/3]).

-record(state, {}).
-define(TIMER, 60000).


%%%%%%%%%
%% API %%
%%%%%%%%%

%% Starts up cache
%% (Module: is for the data val lookup, CacheState: Cache on/off (true/false))
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

get_val(Pid, ID) ->
    gen_server:call(Pid, {get_val, ID}).

search_val(Pid, ID) ->
    gen_server:call(Pid, {search_val, ID}).

%% clears the ID and corresponding val from the cache if it exsists
clear_id(Pid, ID) ->
    gen_server:call(Pid, {clear_cache_id, ID}).

%% clears out the entire cache 
clear_all(Pid) ->
    gen_server:call(Pid, clear_cache_all).

                   
%%%%%%%%%%%%%%%
%% CALLBACKS %%
%%%%%%%%%%%%%%%

init([]) ->
    init_cache(),
    erlang:send_after(?TIMER, self(), trigger),
    {ok, #state{}}.

handle_call({get_val, ID}, _From, #state{} = State ) ->
    Reply = new_val(ID),
    {reply, Reply, State#state {}};
handle_call({search_val, ID}, _From, #state{} = State ) ->
    Reply = lookup(ID),
    {reply, Reply, State#state {}};
handle_call({clear_cache_id, ID}, _From, #state{} = State ) ->
    Reply = clear_cache_id(ID),
    {reply, Reply, State#state {}};
handle_call(clear_cache_all, _From, #state{} = State ) ->
    clear_cache_all(),
    {reply, {ok, cleared_all}, State#state {}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(trigger, State) ->
    clear_cache_all(),
    erlang:send_after(?TIMER, self(), trigger),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%


init_cache() ->
    ets:new(cache_table, [named_table, {read_concurrency, true}, public, {write_concurrency, true}]),
    ok.

new_val(ObjectID) ->
    case hydra_client_test:object_get(ObjectID, undefined) of
	{error, not_found} ->
	    NewVal = null,
	    NewVal;
	{ok, Map} ->
	    [Data, _Type] = maps:to_list(Map),
	    {_, DataMap} = Data,
	    [{_ValData, NewVal}] = maps:to_list(DataMap),
	    ets:insert(cache_table, {ObjectID, NewVal}),
	    NewVal
    end.	    

lookup(ID) ->
    case ets:lookup(cache_table, ID) of
	[] ->
	    {error, ID, not_found};
	[{_ID, Val}] ->
	    {ok, Val}
    end.

clear_cache_id(ID) ->
    case ets:lookup(cache_table, ID) of
	[] -> 
	    {error, ID, not_found};
	[{ID, _Val}] ->
	    ets:delete(cache_table, ID),
	    {ok, ID, removed}
    end.

clear_cache_all() ->
    ets:delete_all_objects(cache_table),
    ok.



