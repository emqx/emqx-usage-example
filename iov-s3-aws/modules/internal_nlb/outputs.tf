output "dns_name" {
  description = "The DNS name of the NLB"
  value       = aws_lb.nlb.dns_name
}

output "security_group_id" {
  value = aws_security_group.nlb_sg.id
}
