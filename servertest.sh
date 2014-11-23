#!/bin/bash
while true; do
    for c in :up :right :down :left; do
        echo $c
        sleep 0.1
    done
done | nc localhost 2048
