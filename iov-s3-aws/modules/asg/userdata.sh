#!/bin/bash

set -x

if which apt >/dev/null 2>&1; then
    apt update -y
    apt install -y curl wget zip unzip net-tools dnsutils ca-certificates gnupg lsb-release jq git python3-pip

    systemctl stop apt-daily.timer
    systemctl disable apt-daily.timer
    systemctl disable apt-daily.service
    systemctl stop apt-daily-upgrade.timer
    systemctl disable apt-daily-upgrade.timer
    systemctl disable apt-daily-upgrade.service

    apt-get purge -y unattended-upgrades
fi

case $(uname -m) in
    x86_64)
        curl -fsSL https://www.emqx.com/en/downloads/MQTTX/v1.11.0/mqttx-cli-linux-x64 -o mqttx-cli-linux
        ;;
    aarch64)
        curl -fsSL https://www.emqx.com/en/downloads/MQTTX/v1.11.0/mqttx-cli-linux-arm64 -o mqttx-cli-linux
        ;;
esac
install ./mqttx-cli-linux /usr/local/bin/mqttx

cat > /etc/sysctl.d/emqx.conf <<EOF
fs.file-max=2097152
fs.nr_open=2097152
EOF

sysctl --load=/etc/sysctl.d/emqx.conf

cat >> /etc/security/limits.conf << EOF
*      soft   nofile      2097152
*      hard   nofile      2097152
EOF

echo 'DefaultLimitNOFILE=2097152' >> /etc/systemd/system.conf

ulimit -n 2097152

mkdir -p /etc/ssl/certs/emqx
echo "${certs.ca}" > /etc/ssl/certs/emqx/cacert.pem
echo "${certs.server_cert}" > /etc/ssl/certs/emqx/cert.pem
echo "${certs.server_key}" > /etc/ssl/certs/emqx/key.pem
echo "${certs.client_cert}" > /etc/ssl/certs/emqx/client-cert.pem
echo "${certs.client_key}" > /etc/ssl/certs/emqx/client-key.pem
# create bundles
cat /etc/ssl/certs/emqx/cert.pem /etc/ssl/certs/emqx/cacert.pem > /etc/ssl/certs/emqx/server-bundle.pem
cat /etc/ssl/certs/emqx/client-cert.pem /etc/ssl/certs/emqx/cacert.pem > /etc/ssl/certs/emqx/client-bundle.pem

${extra}
