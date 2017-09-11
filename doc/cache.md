


__Author:__ Callum Roberts `csr@shopgun.com`

# Module Cache #
* [Function Index](#index)
* [Function Details](#functions)

Dataloader Cache API.


<a name="index"></a>

## Function Index ##

| Function      | Desc          |
|---------------|---------------|
| <a href="#start_link-0">start_link/0</a> | Init cache |
| <a href="#get_val-2">get_val/2</a> | Looks up ID in database and adds returned Val in cache |
| <a href="#search_val-2">search_val/2</a> | takes an ID and searches for the corresponding Val |
| <a href="#clear_id-2">clear_id/2</a> | Clears the ID and corresponding val from the cache if it exsists |
| <a href="#clear_all-1">clear_all/1</a> | Clears out the entire cache  |


<a name="functions"></a>

## Function Details ##



<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> {ok, Pid} | {error, Reason}`

Initilizes the cache process.

This function returns the Pid in which the process is live on.


<a name="get_val-2"></a>

### get_val/2 ###

`get_val(Pid, ID) -> Val | null`

Will return `Val` if ID found or `null` otherwise. The returned value is saved to cache.

This fuction takes the arg of the Pid the Cache is alive on, and an ID value.


<a name="search_val-2"></a>

### search_val/2 ###

`search_val(Pid, ID) -> Val | error`

Useful for testing to see if the cache is storing and clearing.

This function will return the corresponding `Val` from the cache if the ID exsists.

This fuction takes the arg of the Pid the Cache is alive on, and an ID value.


<a name="clear_id-2"></a>

### clear_id/2 ###

`clear_id(Pid, ID) -> {ok, ID, removed} | {error, Reason}`

clears a specific ID from cache.

This function takes the Pid of the live process of the cache and an ID.

Will return `{ok, ID, removed}` on success, else `{error, Reason}`.


<a name="clear_all-1"></a>

### clear_all/1 ###

`clear_all(Pid) -> {ok, cleared_all} | error`

This will clear out the entier cache, the cache is currently automatically cleared every 60 seconds.

This function takes the Pid of the live process of the cache.






