output "dns_name" {
  description = "The DNS name of the load balancer"
  value       = aws_lb.nlb.dns_name
}

output "security_group_id" {
  description = "The ID of the security group"
  value       = aws_security_group.nlb_sg.id
}
