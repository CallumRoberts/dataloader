


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

Will return `Val` if ID found or `null` otherwise, the return value is saved to cache.

This fuction takes the arg of the Pid the Cache is alive on, and an ID value.



## Issues ##

recently discovered that the following functions when used don't clear anything from the cache. 

(fix will be in progress soon)


<a name="clear_id-2"></a>

### clear_id/2 ###



<a name="clear_all-1"></a>

### clear_all/1 ###





