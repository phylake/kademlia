About
=====

The goal is to exercise ØMQ bindings and compare to built-in UDP in order to
make some reasonable observations for reference. A non-goal is to do an apples-
to-apples shootout for performance.

Test
====

Methodology
-----------

Constrain both benchmarks to 1 thread. For Haskell this is a parameter to the
runtime system `+RTS -N1`. For ØMQ it's `setIoThreads 1 ctx`

Test on loopback to factor out the network

Make a best effort to reduce the effect of GC. This was acheived with `+RTS
-A20M` which reduced the garbage collections to ~50 from upwards of 1500

Ensure laziness didn't positively affect the results by **using** the received
data.

Test on a "quiet" machine. This was the output of `top` before the test

```
top - 07:56:15 up 1 day, 14:36,  4 users,  load average: 0.00, 0.01, 0.05
Tasks: 316 total,   1 running, 315 sleeping,   0 stopped,   0 zombie
%Cpu(s):  0.0 us,  0.0 sy,  0.0 ni,100.0 id,  0.0 wa,  0.0 hi,  0.0 si,  0.0 st
KiB Mem:  98967792 total,  2111328 used, 96856464 free,   364596 buffers
KiB Swap: 33518588 total,        0 used, 33518588 free.   781232 cached Mem

  PID USER      PR  NI    VIRT    RES    SHR S  %CPU %MEM     TIME+ COMMAND
    1 root      20   0   33608   2860   1456 S   0.0  0.0   0:04.71 init
    2 root      20   0       0      0      0 S   0.0  0.0   0:00.00 kthreadd
    3 root      20   0       0      0      0 S   0.0  0.0   0:01.34 ksoftirqd/0
    4 root      20   0       0      0      0 S   0.0  0.0   0:07.04 kworker/0:0
    5 root       0 -20       0      0      0 S   0.0  0.0   0:00.00 kworker/0:0H
    6 root      20   0       0      0      0 S   0.0  0.0   0:00.00 kworker/u128:0
    7 root      20   0       0      0      0 S   0.0  0.0   0:00.62 kworker/u129:0
    8 root      20   0       0      0      0 S   0.0  0.0   0:14.54 rcu_sched
```

Results
-------

### Criterion
Find individual results in each of the `readme.md`s.

### strace
Running `strace` revealed ØMQ's
[batching](http://www.aosabook.org/en/zeromq.html) (and probably dozens of other
optimizations I'm unaware of) at work. Here was the count of system calls for
each benchmark:

```
UDP connect/send   135164
UDP       sendTo   145725
ØMQ      PUB/SUB    41747
```

### Feedback
If you have feedback on these tests feel free to open an issue.

Conclusions
-----------

For Kademlia I chose UDP because it matches the spec and was much simpler to
program against.
