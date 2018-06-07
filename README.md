# Ant colony optimization concurrent model with Erlang

The objective of this project is to create a concurrent system to simulate a
population of ants walking between nodes in a graph. Aiming for a realistic
approach, where each edge has a cost of passing through it and this is
translated into a sleeping time for every ant which chooses to pass by to any of
them.

# Useful shell history Erlang/OTP 20

In order to active the obviously useful command history for `erl`, the following
environment variable enables this feature.

```console
$ export ERL_AFLAGS="-kernel shell_history enabled"
```

# How to try this project

```console
$ make

erl> c(main).
{ok,main}
erl> M = main:init(<test_case_file>).
erl> M ! {createAnts, <n>, <start_node_pid>}.
erl> M ! {init}.
```