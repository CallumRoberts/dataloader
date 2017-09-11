

__Author:__ Callum Roberts `csr@shopgun.com`

# Module dataloader #
* [Function Index](#index)
* [Function Details](#functions)

dataloader API.


<a name="index"></a>

## Function Index ##

| Function      | Desc          |
|---------------|---------------|
| <a href="#start_link-0">start_link/0</a> | Init dataloader |
| <a href="#stop-1">stop/1</a> | Stops dataloader  |
| <a href="#batch_load-2">batch_load/2</a> | loads a key / multiple keys, returning a promise |
| <a href="#request_complete-1">request_complete/1</a> | stops batching and replys back to callers |


<a name="functions"></a>

## Function Details ##

<a name="start_link-0"></a>


### start_link/0 ###

`start_link() -> {ok, Pid} | {error, Reason}`

Initilizes a dataloader process.

This function returns the `Pid` in which the process is live on.


<a name="stop-1"></a>

### stop/1 ###

`stop(Pid) -> ok | error`

stops the dataloader alive on specified Pid.

Returns `ok` on successful shutdown of the dataloader process.


<a name="batch_load-2"></a>

### batch_load/2 ###

`batch_load(Pid, [Object]) -> {batch_load_token, Ref} | error`

`Object = {NameToken, ID}`

`NameToken = {token, self(), make_ref()}`

Takes a Pid of a specific dataloader process and a list of Objects.

Returns a promise token on success. 


<a name="request_complete-1"></a>

### request_complete/1 ###

`request_complete(Pid) -> {ok, [Reply]} | error`

`Reply = {ID, Val, NameToken, PromiseToken}`

Returns a list of replys for each Object in the batch load.

`flush()` Can also be used to view what was sent back to the caller.
