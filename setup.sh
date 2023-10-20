doctl auth init --access-token dop_v1_234e9551d15a77a18fae5a883c2d22e5ff671f8bd7e9f303dcaeeee05332e708

cd ~

wget https://github.com/digitalocean/doctl/releases/download/v1.98.1/doctl-1.98.1-linux-amd64.tar.gz

doctl kubernetes cluster kubeconfig save xxxx


kubectl config set-context xxx
kubectl config use-context xxx