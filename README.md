erlpool
================

Erlang round-robin load balancer for Erlang processes based on ETS

What erlpool does
-----------

Erlpool is a round-robin load balancer for Erlang processes meant mainly to be used with things like database connections. 
Compared to [pooler][3] and [poolboy][4], `erlpool` is very simple and small (~100 LOC) with no assumptions about the workers.

Each time you ask for a pid the library is doing over ETS one `counter_update` operation and one `lookup`. This is inspired
from [cuesport][2] which beside this is doing an additional `lookup`. This additional `lookup` in `erlpool` was transformed using 
dynamic compilation in a simple function call. Also another project from where I got inspired is [revolver][1]. 

Quick start
-----------

Getting all deps and compile:

```
rebar get-deps
rebar compile
```

Let's suppose you have a `gen_server` similar with the `benchmark_worker` from benchmark folder. You create a pool for this in the following way:

```erlang 
application:ensure_all_started(erlpool),
Args = [
    {size, 20},
    {start_mfa, {benchmark_worker, start_link, [WorkerArgs]}},
    {supervisor_period, 1},
    {supervisor_intensity, 1000}
],

erlpool:start_pool(pool_name, Args).
```

Or in case you want to use the `sys.config` you can use:

```erlang
[
    {erlpool, [
        {pools, [
            {mypool, [
                {size, 50},
                {group, mygroup},
                {start_mfa, {benchmark_worker, start_link, [ [] ]} },
                {supervisor_period, 1},
                {supervisor_intensity, 1000},
                {supervisor_restart, permanent}
            ]}
        ]}
    ]}
].
```

Arguments:

- `size` : the pool size (how many workers are created and added in the supervisor)
- `group`: used to group multiple pools in a group. For example you have two different apps that are using erlpool and you want to restart/delete the 
pools for app1. Using groups you can delete/restart all pools for a certain application. 
- `smart_mfa` : Defines the function call used to start the child process. It must be a module-function-arguments tuple `{M,F,A}` used as `apply(M,F,A)`
- `supervisor_period` : the supervisor restart period in seconds (default to 1)
- `supervisor_intensity` : the supervisor restart intensity (defaults to 100)
- `supervisor_restart` : the supervisor restart strategy (permanent (default) | transient | temporary)

If you are not familiar with supervisor settings check the [documentation][5]. Basically to prevent a supervisor from getting 
into an infinite loop of child process terminations and restarts, a maximum restart intensity is defined using two integer values. 
Assuming the values `supervisor_intensity` and `supervisor_period`, then, if more than `supervisor_intensity` restarts occur within 
`supervisor_period` seconds, the supervisor terminates all child processes and then itself. The intensity defaults to 100 and period defaults to 1.

API
-------

#### Create a pool at runtime

```erlang 
Args = [
    {size, 20},
    {start_mfa, {benchmark_worker, start_link, [WorkerArgs]}},
    {supervisor_period, 1},
    {supervisor_intensity, 1000}
],

ok = erlpool:start_pool(pool_name, Args).
```

#### Remove a pool at runtime

```erlang
ok = erlpool:stop_pool(pool_name).
```

#### Restart a pool

Use the following function when you want to restart a pool

```
ok = erlpool:restart_pool(pool_name).
```

#### Get a pid from pool:

To get a pid from a pool in a round robbin fashion you are using:

```erlang
Pid = erlpool:pid(pool_name).
```

#### Run a function over all pid's in a pool:

In case you want to run a function over all pid's in the pool you can use the `map/2` function. For example the following
function returns all pid's in a list:

```erlang
erlpool:map(pool_name, fun(Pid) -> Pid end).
```

#### Stop all pools in a group

To remove all pools in a group use: 

```erlang
 erlpool:stop_group(group_name).
```

#### Restart all pools in a group

To restart all pools in a group use:

```erlang
 erlpool:restart_group(group_name).
```

Performance testing
-----------

The code is in `benchmark` folder. The test sends 100000 requests from 4000 concurrent processes to a gen_server that 
replies with ok. The pools has 20 workers.

```
erl -pa ebin -pa deps/*/ebin -noshell -eval "bench_pool:bench(100000, 4000)." -eval "init:stop()."
### erlpool 351 ms 333333 req/sec 
### cuesport 1412 ms 71428 req/sec 
### revolver 1165 ms 90909 req/sec 
### poolboy 1806 ms 55555 req/sec 
### pooler 3609 ms 27777 req/sec 
```

You can run it yourself using `make bench` after you copy the sources from benchmark folder in src and compile. 
You need to uncomment also the benchmarks dependencies in `rebar.config`

[1]:https://github.com/odo/revolver
[2]:https://github.com/esl/cuesport
[3]:https://github.com/seth/pooler
[4]:https://github.com/devinus/poolboy
[5]:http://erlang.org/doc/man/supervisor.html
