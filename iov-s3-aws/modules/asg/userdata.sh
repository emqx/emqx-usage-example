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
fi

token=$(curl -sSL -X PUT --retry 40 --retry-connrefused --retry-delay 5 "http://169.254.169.254/latest/api/token" -H "X-aws-ec2-metadata-token-ttl-seconds: 180")
curl -sSL --retry 40 --retry-connrefused --retry-delay 5 -H "X-aws-ec2-metadata-token: $token" http://169.254.169.254/latest/dynamic/instance-identity/document > /tmp/instance-identity.json

wget -q https://awscli.amazonaws.com/awscli-exe-linux-$(uname -m).zip -O /tmp/awscliv2.zip
unzip -q /tmp/awscliv2.zip -d /tmp
/tmp/aws/install

case $(uname -m) in
    x86_64)
        curl -fsSL https://www.emqx.com/en/downloads/MQTTX/v1.11.0/mqttx-cli-linux-x64 -o mqttx-cli-linux
        ;;
    aarch64)
        curl -fsSL https://www.emqx.com/en/downloads/MQTTX/v1.11.0/mqttx-cli-linux-arm64 -o mqttx-cli-linux
        ;;
esac
install ./mqttx-cli-linux /usr/local/bin/mqttx

cat > /etc/sysctl.d/99-emqx.conf <<EOF
fs.file-max=2097152
fs.nr_open=2097152
net.ipv4.ip_local_port_range=1025 65535
net.core.somaxconn=32768
net.ipv4.tcp_max_syn_backlog=16384
net.core.netdev_max_backlog=16384
net.core.rmem_default=262144
net.core.wmem_default=262144
net.core.rmem_max=16777216
net.core.wmem_max=16777216
net.core.optmem_max=16777216
net.ipv4.tcp_fin_timeout=15
EOF

sysctl --load=/etc/sysctl.d/99-emqx.conf

cat >> /etc/security/limits.conf << EOF
*      soft   nofile      2097152
*      hard   nofile      2097152
EOF

echo 'DefaultLimitNOFILE=2097152' >> /etc/systemd/system.conf

ulimit -n 2097152

swapoff -a

mkdir -p /etc/ssl/certs/emqx
echo "${certs.ca}" > /etc/ssl/certs/emqx/cacert.pem
echo "${certs.server_cert}" > /etc/ssl/certs/emqx/cert.pem
echo "${certs.server_key}" > /etc/ssl/certs/emqx/key.pem
echo "${certs.client_cert}" > /etc/ssl/certs/emqx/client-cert.pem
echo "${certs.client_key}" > /etc/ssl/certs/emqx/client-key.pem
# create bundles
cat /etc/ssl/certs/emqx/cert.pem /etc/ssl/certs/emqx/cacert.pem > /etc/ssl/certs/emqx/server-bundle.pem
cat /etc/ssl/certs/emqx/client-cert.pem /etc/ssl/certs/emqx/cacert.pem > /etc/ssl/certs/emqx/client-bundle.pem

if [ ${register_hostname} == "true" ]; then
    hostnamectl set-hostname ${hostname}
    private_ip=$(curl -fsSL -H "X-aws-ec2-metadata-token: $token" http://169.254.169.254/latest/meta-data/local-ipv4)
    cat > /tmp/change-resource-record-sets.json <<EOF
{ "Changes": [ { "Action": "UPSERT", "ResourceRecordSet": { "Name": "${hostname}", "Type": "A", "TTL": 60, "ResourceRecords": [ { "Value": "$private_ip" } ] } } ] }
EOF
    aws route53 change-resource-record-sets --hosted-zone-id ${hosted_zone_id} --change-batch file:///tmp/change-resource-record-sets.json
fi

${extra}
