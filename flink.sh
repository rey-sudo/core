kubectl create -f https://github.com/jetstack/cert-manager/releases/download/v1.8.2/cert-manager.yaml

helm repo add flink-kubernetes-operator-1.3.1 https://archive.apache.org/dist/flink/flink-kubernetes-operator-1.3.1/

helm install flink-kubernetes-operator flink-kubernetes-operator-1.3.1/flink-kubernetes-operator \
--set webhook.create=false --set image.repository=apache/flink-kubernetes-operator