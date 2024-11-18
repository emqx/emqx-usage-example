output "certs" {
  value = {
    ca          = tls_self_signed_cert.ca_cert.cert_pem
    server_cert = tls_locally_signed_cert.server_cert.cert_pem
    server_key  = tls_private_key.server_key.private_key_pem
    client_cert = tls_locally_signed_cert.client_cert.cert_pem
    client_key  = tls_private_key.client_key.private_key_pem
  }
}
