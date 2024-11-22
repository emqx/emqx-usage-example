resource "aws_lb" "nlb" {
  name                             = "${var.prefix}-internal-lb"
  internal                         = true
  load_balancer_type               = "network"
  subnets                          = var.subnet_ids
  security_groups                  = [aws_security_group.nlb_sg.id]
  enable_cross_zone_load_balancing = true
}

resource "aws_lb_listener" "mqtt" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = 1883
  protocol          = "TCP"
  default_action {
    target_group_arn = var.mqtt_target_group_arn
    type             = "forward"
  }
}

resource "aws_lb_listener" "mqtts" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = 8883
  protocol          = "TCP"
  default_action {
    target_group_arn = var.mqtts_target_group_arn
    type             = "forward"
  }
}

resource "aws_lb_listener" "api" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = 18083
  protocol          = "TCP"
  default_action {
    target_group_arn = var.api_target_group_arn
    type             = "forward"
  }
}

resource "aws_security_group" "nlb_sg" {
  name_prefix = var.prefix
  description = "Access to MQTT port"
  vpc_id      = var.vpc_id

  dynamic "ingress" {
    for_each = [1883, 8883, 18083]
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
