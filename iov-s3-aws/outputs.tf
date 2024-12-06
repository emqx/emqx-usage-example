output "emqx_dashboard_url" {
  description = "EMQX Dashboard URL"
  value       = "http://${module.public_nlb.dns_name}:18083/"
}
