# output "emqx_dashboard_url" {
#   description = "EMQX Dashboard URL"
#   value       = "http://${module.public_nlb.dns_name}:18083"
# }

# output "grafana_url" {
#   description = "Grafana URL"
#   value       = local.monitoring_enabled ? "http://${module.public_nlb.dns_name}:3000" : null
# }

# output "prometheus_url" {
#   description = "Prometheus URL"
#   value       = local.monitoring_enabled ? "http://${module.public_nlb.dns_name}:9090" : null
# }

# output "emqx_dashboard_credentials" {
#   description = "EMQX Dashboard credentials"
#   value       = "admin:${local.emqx_dashboard_default_password}"
# }

# output "grafana_credentials" {
#   description = "Grafana credentials"
#   value       = local.monitoring_enabled ? "admin:grafana" : null
# }

# output "emqx_nodes" {
#   description = "EMQX nodes"
#   value       = [for node in module.emqx : { ip : node.public_ips[0], fqdn : node.fqdn }]
# }

# output "loadgen_nodes" {
#   description = "loadgen nodes"
#   value       = [for node in module.loadgen : { ip : node.public_ips[0], fqdn : node.fqdn }]
# }

# output "emqttb_nodes" {
#   description = "emqttb nodes"
#   value       = [for node in module.loadgen : { ip : node.public_ips[0], fqdn : node.fqdn } if node.type == "emqttb"]
# }

# output "emqtt_bench_nodes" {
#   description = "emqtt bench nodes"
#   value       = [for node in module.loadgen : { ip : node.public_ips[0], fqdn : node.fqdn } if node.type == "emqtt_bench"]
# }

# output "http_nodes" {
#   description = "http nodes"
#   value       = [for node in module.integration : { ip : node.public_ips[0], fqdn : node.fqdn } if node.type == "http"]
# }

# output "rabbitmq_nodes" {
#   description = "rabbitmq nodes"
#   value       = [for node in module.integration : { ip : node.public_ips[0], fqdn : node.fqdn } if node.type == "rabbitmq"]
# }

# output "monitoring_nodes" {
#   description = "monitoring nodes"
#   value       = [for node in module.monitoring : { ip : node.public_ips[0], fqdn : node.fqdn }]
# }

output "ssh_key_path" {
  description = "SSH key path"
  value       = local.ssh_key_path
}
