# Generate private key for the CA
resource "tls_private_key" "ca_key" {
  algorithm = "ED25519"
}

# Generate a self-signed CA certificate
resource "tls_self_signed_cert" "ca_cert" {
  private_key_pem = tls_private_key.ca_key.private_key_pem

  subject {
    common_name  = "${var.subject.cn} CA"
    organization = var.subject.o
    country      = var.subject.c
  }

  validity_period_hours = 87600 # 10 years
  is_ca_certificate     = true
  allowed_uses = [
    "cert_signing",
    "key_encipherment",
    "digital_signature"
  ]
}

resource "local_file" "ca_key_file" {
  filename = "certs/ca_key.pem"
  content  = tls_private_key.ca_key.private_key_pem
}

resource "local_file" "ca_cert_file" {
  filename = "certs/cacert.pem"
  content  = tls_self_signed_cert.ca_cert.cert_pem
}

# Generate private key for the server
resource "tls_private_key" "server_key" {
  algorithm = "ED25519"
}

# Generate a Certificate Signing Request (CSR) for the server
resource "tls_cert_request" "server_csr" {
  private_key_pem = tls_private_key.server_key.private_key_pem

  subject {
    common_name  = var.subject.cn
    organization = var.subject.o
    country      = var.subject.c
  }
}

# Sign the server certificate with the CA
resource "tls_locally_signed_cert" "server_cert" {
  cert_request_pem   = tls_cert_request.server_csr.cert_request_pem
  ca_private_key_pem = tls_private_key.ca_key.private_key_pem
  ca_cert_pem        = tls_self_signed_cert.ca_cert.cert_pem

  validity_period_hours = 8760 # 1 year

  allowed_uses = [
    "key_encipherment",
    "digital_signature",
    "server_auth"
  ]
}

resource "local_file" "server_key_file" {
  filename = "certs/key.pem"
  content  = tls_private_key.server_key.private_key_pem
}

resource "local_file" "server_cert_file" {
  filename = "certs/cert.pem"
  content  = tls_locally_signed_cert.server_cert.cert_pem
}

resource "local_file" "server_bundle" {
  filename = "certs/server-bundle.pem"
  content  = "${tls_locally_signed_cert.server_cert.cert_pem}\n${tls_self_signed_cert.ca_cert.cert_pem}"
}

# Generate private key for the client
resource "tls_private_key" "client_key" {
  algorithm = "ED25519"
}

# Generate a Certificate Signing Request (CSR) for the client
resource "tls_cert_request" "client_csr" {
  private_key_pem = tls_private_key.client_key.private_key_pem

  subject {
    common_name  = var.subject.cn
    organization = var.subject.o
    country      = var.subject.c
  }
}

# Sign the client certificate with the CA
resource "tls_locally_signed_cert" "client_cert" {
  cert_request_pem   = tls_cert_request.client_csr.cert_request_pem
  ca_private_key_pem = tls_private_key.ca_key.private_key_pem
  ca_cert_pem        = tls_self_signed_cert.ca_cert.cert_pem

  validity_period_hours = 8760 # 1 year

  allowed_uses = [
    "key_encipherment",
    "digital_signature",
    "client_auth"
  ]
}

resource "local_file" "client_key_file" {
  filename = "certs/client-key.pem"
  content  = tls_private_key.client_key.private_key_pem
}

resource "local_file" "client_cert_file" {
  filename = "certs/client-cert.pem"
  content  = tls_locally_signed_cert.client_cert.cert_pem
}

resource "local_file" "client_bundle" {
  filename = "certs/client-bundle.pem"
  content  = "${tls_locally_signed_cert.client_cert.cert_pem}\n${tls_self_signed_cert.ca_cert.cert_pem}"
}
