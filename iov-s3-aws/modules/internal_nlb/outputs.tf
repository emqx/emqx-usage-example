output "dns_name" {
  description = "The DNS name of the NLB"
  value       = aws_lb.nlb.dns_name
}

output "security_group_id" {
  value = aws_security_group.nlb_sg.id
}

output "mqtt_target_group_arn" {
  value = aws_lb_target_group.mqtt.arn
}

output "mqtts_target_group_arn" {
  value = aws_lb_target_group.mqtts.arn
}

output "httpapi_target_group_arn" {
  value = aws_lb_target_group.httpapi.arn
}
