-module(dataloader).
-behaviour(gen_server).

-export([start_link/0, stop/1, batch_load/2, request_complete/1]).
 
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
 	 code_change/3]).

-record(state, { promise_map }).
-define(TIMER, 300). 


%%%%%%%%%
%% API %%
%%%%%%%%%

%% Starts up new dataloader 
start_link() ->
    gen_server:start_link( ?MODULE, [], []).

%% Stops the dataloader
stop(Pid) ->
    gen_server:stop(Pid).

%% loads a key/multiple keys, promissing an array of values
batch_load(Pid, Object) ->
    gen_server:call(Pid, {batch_load, Object}).

%% stops batching and replys back to callers
request_complete(Pid) ->
    gen_server:call(Pid, sync).
    
    
%%%%%%%%%%%%%%%
%% CALLBACKS %%
%%%%%%%%%%%%%%%

init([]) ->
    {ok, #state{ promise_map = #{} }}.

handle_call(sync, _From, #state {} = State ) ->
    {Reply, NewState} = handle_sync(State),
    {reply, {ok, Reply}, NewState#state {}};
handle_call({batch_load, Object}, _From, #state { promise_map = Map } = State) ->
    Token = generate_token(), 
    TimerRef = erlang:send_after(?TIMER, self(), timeout),
    Fun = (fun(Objs) -> {Objs, Token} end),
    Entries = lists:map(Fun, Object),              
    Promise = store_promise(Entries, Map, TimerRef),
    Reply = {batch_load_token, Token},
    {reply, Reply, State#state { promise_map = Promise }};
handle_call(_, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) -> 
    {noreply, State}.

handle_info(timeout, #state {} = State ) ->
    {_Reply, NewState} = handle_sync(State),
    {noreply, NewState#state {}}.

terminate(Reason, State) ->
    {ok, server_terminating, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTIONS %%
%%%%%%%%%%%%%%%%%%%%%%%%

generate_token() ->
    make_ref().

store_promise([], Map, TimerRef) -> erlang:cancel_timer(TimerRef),
				Map;
store_promise([{Object, Token} | Entries], Map, TimerRef) ->  
    case maps:find(Object, Map) of 
	error ->
	    store_promise(Entries, Map#{ Object => [Token] }, TimerRef);
	{ok, OtherTokens} ->
	    store_promise(Entries, Map#{ Object := [Token | OtherTokens] }, TimerRef)
    end.

handle_sync(State = #state{ promise_map = Map }) ->
    ListedData = maps:to_list(Map),
    Reply = get_reply(ListedData, [], ets_cache),
    ok = reply_back(Reply),
    NewState = State#state { promise_map = #{} },
    {Reply, NewState}.

get_reply([], List, _CachePid) ->
    List;
get_reply([Req | OtherReq], List, CachePid) ->
    {Object, Token} = Req,
    {NameToken, ID} = Object,
    case ets:lookup(cache_table, ID) of 
	[] -> 
	    NewVal = ets_cache:get_val(CachePid, ID),
	    NewList = [{ID, NewVal, NameToken, Token}] ++ List;
	[{ID, Val}] -> 
	    Val,
	    NewList = [{ID, Val, NameToken, Token}] ++ List				
    end,
    get_reply(OtherReq, NewList, CachePid).
    	   	    
reply_back([]) -> ok;
reply_back([{ID, Val, NameToken, Token} | Data]) ->
    ok = reply_back_callers(ID, Val, NameToken, Token),
    reply_back(Data).

reply_back_callers(_ID, _Val, _NameToken, []) ->
    ok;
reply_back_callers(ID, Val, NameToken, [_Token | OtherTokens]) ->
    graphql:reply_cast(NameToken, Val),
    reply_back_callers(ID, Val, NameToken, OtherTokens).









