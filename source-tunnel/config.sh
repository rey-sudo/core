#!/bin/bash
kubectl create cm seatunnel-config \
--from-file=seatunnel.streaming.conf=seatunnel.streaming.conf