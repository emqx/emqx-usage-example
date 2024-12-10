#!/bin/bash

set -x

systemctl start snap.amazon-ssm-agent.amazon-ssm-agent.service

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
elif which dnf >/dev/null 2>&1; then
    dnf install -y curl wget zip unzip net-tools bind-utils ca-certificates gnupg jq git python3
elif which yum >/dev/null 2>&1; then
    yum install -y curl wget zip unzip net-tools bind-utils ca-certificates gnupg jq git python3
fi

cat > /etc/sysctl.d/99-custom.conf <<EOF
fs.file-max=2097152
fs.nr_open=2097152
net.ipv4.ip_local_port_range=1025 65535
net.ipv4.tcp_tw_reuse=1
net.ipv4.tcp_fin_timeout=15
EOF

sysctl --load=/etc/sysctl.d/99-custom.conf

cat >> /etc/security/limits.conf << EOF
*      soft   nofile      2097152
*      hard   nofile      2097152
EOF

echo 'DefaultLimitNOFILE=2097152' >> /etc/systemd/system.conf

ulimit -n 2097152

swapoff -a

token=$(curl -sSL -X PUT --retry 40 --retry-connrefused --retry-delay 5 "http://169.254.169.254/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 180")
curl -sSL --retry 40 --retry-connrefused --retry-delay 5 -H "X-aws-ec2-metadata-token: $token" http://169.254.169.254/latest/dynamic/instance-identity/document > /tmp/instance-identity.json

wget -q https://awscli.amazonaws.com/awscli-exe-linux-$(uname -m).zip -O /tmp/awscliv2.zip
unzip -q /tmp/awscliv2.zip -d /tmp
/tmp/aws/install

[ -n "${hostname}" ] && hostnamectl set-hostname ${hostname}

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

touch /opt/tf_init_done
