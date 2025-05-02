
notes about system limits in Linux:
- `ulimit -n 100000` will raise the max number of FDs for a process to 100000
- `/proc/sys/net/core/netdev_max_backlog` controls the kernel backlog size, raise it (default is 1000)
- `/proc/sys/net/core/somaxconn` is the max size of a socket backlog (as given to `listen()`), raise it (default is 4096)

