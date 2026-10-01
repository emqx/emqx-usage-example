#!/bin/sh
set -eu

if [ -f /ca/auth-ca.pem ]; then
    for tenant in north south; do
        openssl x509 -checkend 0 -noout -in "/$tenant/server.pem"
    done
    echo "Using existing demo certificates. Use docker compose down -v to regenerate."
    exit 0
fi

# The CA private key stays in this short-lived container, outside the volumes.
umask 077
openssl req -x509 -newkey rsa:2048 -nodes -days 30 \
    -keyout /tmp/ca.key -out /tmp/ca.pem \
    -subj '/CN=Dynamic Tenant Auth Demo CA' \
    -addext 'basicConstraints=critical,CA:TRUE' \
    -addext 'keyUsage=critical,keyCertSign,cRLSign'

for tenant in north south; do
    sans="DNS:$tenant.auth.example.com"
    # Give the negative-test alias valid DNS and TLS; only allowed_hosts blocks it.
    if [ "$tenant" = north ]; then
        sans="$sans,DNS:west.auth.example.com"
    fi
    openssl req -new -newkey rsa:2048 -nodes \
        -keyout "/$tenant/server.key" -out "/tmp/$tenant.csr" \
        -subj "/CN=$tenant.auth.example.com"
    cat > "/tmp/$tenant.ext" <<EOF
basicConstraints=critical,CA:FALSE
keyUsage=critical,digitalSignature,keyEncipherment
extendedKeyUsage=serverAuth
subjectAltName=$sans
EOF
    openssl x509 -req -days 30 -in "/tmp/$tenant.csr" \
        -CA /tmp/ca.pem -CAkey /tmp/ca.key -CAcreateserial \
        -extfile "/tmp/$tenant.ext" -out "/$tenant/server.pem"
    chmod 644 "/$tenant/server.pem"
done

cp /tmp/ca.pem /ca/auth-ca.pem
chmod 644 /ca/auth-ca.pem
echo "Created a demo CA and separate north/south server certificates."
