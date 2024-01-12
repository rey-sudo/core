kubectl create -f 'https://strimzi.io/install/latest?namespace=default' -n default
kubectl create -f 'https://strimzi.io/install/latest?namespace=kafka' -n kafka
kubectl apply -f https://strimzi.io/examples/latest/kafka/kafka-persistent.yaml -n default 


