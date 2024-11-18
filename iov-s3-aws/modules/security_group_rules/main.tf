resource "aws_security_group_rule" "allow_all_inbound_from_self" {
  type              = "ingress"
  security_group_id = var.security_group_id
  self              = true
  protocol          = "-1"
  from_port         = 0
  to_port           = 0
}

resource "aws_security_group_rule" "allow_all_inbound_from_vpc" {
  type              = "ingress"
  security_group_id = var.security_group_id
  cidr_blocks       = [var.cidr_ipv4]
  protocol          = "-1"
  from_port         = 0
  to_port           = 0
}

resource "aws_security_group_rule" "allow_ssh_access" {
  type              = "ingress"
  security_group_id = var.security_group_id
  cidr_blocks       = ["0.0.0.0/0"]
  protocol          = "TCP"
  from_port         = 22
  to_port           = 22
}

resource "aws_security_group_rule" "allow_all_outbound_ipv4" {
  type              = "egress"
  security_group_id = var.security_group_id
  cidr_blocks       = ["0.0.0.0/0"]
  protocol          = "-1"
  from_port         = 0
  to_port           = 0
}

resource "aws_security_group_rule" "allow_all_outbound_ipv6" {
  type              = "egress"
  security_group_id = var.security_group_id
  ipv6_cidr_blocks  = ["::/0"]
  protocol          = "-1"
  from_port         = 0
  to_port           = 0
}
