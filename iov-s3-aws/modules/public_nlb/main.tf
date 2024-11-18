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
  port              = "18083"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.emqx-dashboard.arn
  }
}

resource "aws_lb_listener" "mqtt" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "1883"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.emqx-mqtt.arn
  }
}

resource "aws_lb_listener" "mqtts" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "8883"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.emqx-mqtts.arn
  }
}

resource "aws_lb_listener" "emqx-ws" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "8083"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.emqx-ws.arn
  }
}

resource "aws_lb_listener" "emqx-wss" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "8084"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.emqx-wss.arn
  }
}

resource "aws_lb_listener" "grafana" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "3000"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.grafana.arn
  }
}

resource "aws_lb_listener" "prometheus" {
  load_balancer_arn = aws_lb.nlb.arn
  port              = "9090"
  protocol          = "TCP"

  default_action {
    type             = "forward"
    target_group_arn = aws_lb_target_group.prometheus.arn
  }
}

resource "aws_lb_target_group" "emqx-dashboard" {
  name        = "${var.prefix}-emqx"
  port        = 18083
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_lb_target_group" "emqx-mqtt" {
  name        = "${var.prefix}-emqx-mqtt"
  port        = 1883
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_lb_target_group" "emqx-mqtts" {
  name        = "${var.prefix}-emqx-mqtts"
  port        = 8883
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_lb_target_group" "emqx-ws" {
  name        = "${var.prefix}-emqx-ws"
  port        = 8083
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_lb_target_group" "emqx-wss" {
  name        = "${var.prefix}-emqx-wss"
  port        = 8084
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_lb_target_group" "grafana" {
  name        = "${var.prefix}-grafana"
  port        = 3000
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_lb_target_group" "prometheus" {
  name        = "${var.prefix}-prometheus"
  port        = 9090
  protocol    = "TCP"
  target_type = "instance"
  vpc_id      = var.vpc_id
}

resource "aws_security_group" "nlb_sg" {
  name_prefix = var.prefix
  description = "Access to EMQX Dashboard, Grafana and Prometheus"
  vpc_id      = var.vpc_id

  dynamic "ingress" {
    for_each = [18083, 1883, 8883, 8083, 8084, 3000, 9090]
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
