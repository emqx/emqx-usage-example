output "dns_name" {
  description = "The DNS name of the load balancer"
  value       = aws_lb.nlb.dns_name
}

output "security_group_id" {
  description = "The ID of the security group"
  value       = aws_security_group.nlb_sg.id
}

output "emqx_dashboard_target_group_arn" {
  value = aws_lb_target_group.emqx-dashboard.arn
}

output "emqx_mqtt_target_group_arn" {
  value = aws_lb_target_group.emqx-mqtt.arn
}

output "emqx_mqtts_target_group_arn" {
  value = aws_lb_target_group.emqx-mqtts.arn
}

output "emqx_ws_target_group_arn" {
  value = aws_lb_target_group.emqx-ws.arn
}

output "emqx_wss_target_group_arn" {
  value = aws_lb_target_group.emqx-wss.arn
}

output "grafana_target_group_arn" {
  value = aws_lb_target_group.grafana.arn
}

output "prometheus_target_group_arn" {
  value = aws_lb_target_group.prometheus.arn
}
