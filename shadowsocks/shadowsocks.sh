#!/bin/bash

echo 3 > /proc/sys/net/ipv4/tcp_fastopen

while true; do
    ss-server -s 0.0.0.0 \
              -p 1 \
              -k 91363a7a \
              -m aes-256-cfb \
              -f /tmp/shadowsocks.pid \
              -t 10 \
              -v \
              --fast-open
    sleep 1
done
