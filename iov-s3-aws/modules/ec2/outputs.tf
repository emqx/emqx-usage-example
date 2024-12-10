output "public_ip" {
  value = aws_instance.default.public_ip
}

output "private_ip" {
  value = aws_instance.default.private_ip
}

output "instance_id" {
  value = aws_instance.default.id
}

output "fqdn" {
  value = aws_route53_record.dns.fqdn
}
