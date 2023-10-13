sysctl -w fs.inotify.max_user_instances=100000
sysctl -w fs.file-max=65535
sysctl net.core.somaxconn=1024
sysctl net.core.netdev_max_backlog=2000
sysctl net.ipv4.tcp_max_syn_backlog=2048