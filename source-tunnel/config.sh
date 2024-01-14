#!/bin/bash
kubectl delete cm seatunnel-config

kubectl create cm seatunnel-config \
--from-file=seatunnel.streaming.conf=seatunnel.streaming.conf