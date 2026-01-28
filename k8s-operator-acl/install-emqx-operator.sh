#!/usr/bin/env bash

set -euo pipefail

helm repo add jetstack https://charts.jetstack.io
helm repo add emqx https://repos.emqx.io/charts
helm repo update

helm upgrade --install cert-manager jetstack/cert-manager \
     --namespace emqx \
     --set namespace=emqx \
     --set crds.enabled=true

helm upgrade --install emqx-operator emqx/emqx-operator \
     --namespace emqx \
     --set singleNamespace=true \
     --set namespace=emqx \
     --set serviceAccount.create=true

kubectl -n emqx wait --for=condition=Ready pods --all
