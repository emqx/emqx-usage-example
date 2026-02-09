# EMQX Operator ACL Configuration

This example demonstrates how to deploy EMQX on Kubernetes using the EMQX Operator with file-based ACL (Access Control List) rules mounted via a ConfigMap.

## Overview

The setup includes:
- A ConfigMap containing ACL rules (`acl.conf`)
- An EMQX custom resource that mounts the ACL file to both core and replicant nodes

## ACL Rules

The example ACL configuration:
- Allows the `dashboard` user to subscribe to `$SYS/#`
- Allows localhost (`127.0.0.1`) to subscribe to `$SYS/#` and `#`
- Allows all clients to subscribe to `foobar/#`
- Denies all clients from subscribing to `$SYS/#` and wildcard `#`
- Denies everything else by default

## Prerequisites

- Kubernetes cluster
- `kubectl` configured
- `helm` installed

## Quick Start

1. **Create the namespace:**
   ```bash
   kubectl create namespace emqx
   ```

2. **Install EMQX Operator and cert-manager:**
   ```bash
   ./install-emqx-operator.sh
   ```

3. **Deploy EMQX with ACL configuration:**
   ```bash
   kubectl apply -f emqx.yaml
   ```

4. **Wait for EMQX to be ready:**
   ```bash
   kubectl -n emqx wait --for=condition=Ready emqx/emqx --timeout=300s
   ```

5. **Verify the deployment:**
   ```bash
   kubectl -n emqx get pods
   ```

## Files

- `install-emqx-operator.sh`: Script to install cert-manager and EMQX Operator via Helm
- `emqx.yaml`: Kubernetes manifests including the ACL ConfigMap and EMQX custom resource

## Notes

- The ACL file is mounted to `/opt/emqx/etc/acl.conf` on both core and replicant nodes
- To modify ACL rules, update the ConfigMap and restart the pods or use EMQX's hot configuration reload
