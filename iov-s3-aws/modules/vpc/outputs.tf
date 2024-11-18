output "vpc_id" {
  value = aws_vpc.vpc.id
}

output "region" {
  value = var.vpc_region
}

output "vpc_cidr_block" {
  value = aws_vpc.vpc.cidr_block
}

output "public_subnet_ids" {
  value = aws_subnet.public[*].id
}

output "private_subnet_ids" {
  value = aws_subnet.private[*].id
}

output "main_route_table_id" {
  value = aws_vpc.vpc.main_route_table_id
}

output "security_group_id" {
  value = aws_security_group.vpc_sg.id
}

output "key_name" {
  value = aws_key_pair.kp.key_name
}
