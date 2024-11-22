resource "aws_lb" "nlb" {
  name                             = "${var.prefix}-public-lb"
  internal                         = false
  load_balancer_type               = "network"
  subnets                          = var.subnet_ids
  security_groups                  = [aws_security_group.nlb_sg.id]
  enable_cross_zone_load_balancing = true
}

resource "aws_lb_listener" "emqx-dashboard" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = 443
  protocol          = "TLS"
  ssl_policy        = "ELBSecurityPolicy-TLS-1-2-2017-01"

  certificate_arn = aws_acm_certificate_validation.validation.certificate_arn

  default_action {
    type             = "forward"
    target_group_arn = var.dashboard_target_group_arn
  }
}

resource "aws_lb_listener" "mqtts" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "8883"
  protocol          = "TLS"
  ssl_policy        = "ELBSecurityPolicy-TLS-1-2-2017-01"

  certificate_arn = aws_acm_certificate_validation.validation.certificate_arn

  default_action {
    type             = "forward"
    target_group_arn = var.mqtts_target_group_arn
  }
}

resource "aws_lb_listener" "emqx-wss" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "8084"
  protocol          = "TLS"
  ssl_policy        = "ELBSecurityPolicy-TLS-1-2-2017-01"

  certificate_arn = aws_acm_certificate_validation.validation.certificate_arn

  default_action {
    type             = "forward"
    target_group_arn = var.wss_target_group_arn
  }
}

resource "aws_security_group" "nlb_sg" {
  name_prefix = var.prefix
  description = "Access to EMQX Dashboard, MQTT and WSS ports"
  vpc_id      = var.vpc_id

  dynamic "ingress" {
    for_each = [443, 8883, 8084]
    content {
      from_port        = ingress.value
      to_port          = ingress.value
      protocol         = "TCP"
      cidr_blocks      = ["0.0.0.0/0"]
      ipv6_cidr_blocks = ["::/0"]
    }
  }

  egress {
    from_port   = 0
    to_port     = 0
    protocol    = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

resource "aws_acm_certificate" "certificate" {
  domain_name       = var.domain_name
  validation_method = "DNS"

  tags = {
    Name = var.prefix
  }
}

resource "aws_route53_record" "validation" {
  for_each = {
    for dvo in aws_acm_certificate.certificate.domain_validation_options : dvo.domain_name => {
      name   = dvo.resource_record_name
      record = dvo.resource_record_value
      type   = dvo.resource_record_type
    }
  }

  allow_overwrite = true
  name            = each.value.name
  records         = [each.value.record]
  ttl             = 60
  type            = each.value.type
  zone_id         = var.route53_zone_id
}

resource "aws_acm_certificate_validation" "validation" {
  certificate_arn         = aws_acm_certificate.certificate.arn
  validation_record_fqdns = [for r in aws_route53_record.validation : r.fqdn]
}

resource "aws_route53_record" "nlb" {
  zone_id = var.route53_zone_id
  name    = var.domain_name
  type    = "A"

  alias {
    name                   = aws_lb.nlb.dns_name
    zone_id                = aws_lb.nlb.zone_id
    evaluate_target_health = false
  }
}
