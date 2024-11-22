output "emqx_dashboard_url" {
  description = "EMQX Dashboard URL"
  value       = "https://${local.public_lb_domain_name}/"
}
