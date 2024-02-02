doctl auth init --access-token dop_v1_132ac040845d213404a36f2f211f3d355fc68dad5261ead3a42f11d1ce7f4367

#cd ~

#wget https://github.com/digitalocean/doctl/releases/download/v1.98.1/doctl-1.98.1-linux-amd64.tar.gz

doctl kubernetes cluster kubeconfig save 83ffbc45-db61-4dcf-94b2-501994cb7c2c

kubectl config set-context do-nyc1-k8arka
kubectl config use-context do-nyc1-k8arka







#kubectl get nodes -o wide      docker internal ip