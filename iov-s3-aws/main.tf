terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "5.56.1"
    }
  }
}

provider "aws" {
  region = var.region
  default_tags {
    tags = {
      Name        = var.prefix
      Environment = "demo"
    }
  }
}

locals {
  route53_zone_name = replace("${var.prefix}.emqx.io", "/", "-")
  public_lb_domain_name = "${var.prefix}.${var.public_domain_name}"
  static_seeds = [
    for i in range(0, 2) : "emqx@emqx-core-${i}.${local.route53_zone_name}"
  ]
  emqx_public_target_groups = {
    dashboard = {
      name = "${var.prefix}-public-dashboard"
      port = 18083
    }
    mqtts = {
      name = "${var.prefix}-public-mqtts"
      port = 8883
    }
    wss = {
      name = "${var.prefix}-public-wss"
      port = 8084
    }
  }

  emqx_private_target_groups = {
    mqtt = {
      name = "${var.prefix}-private-mqtt"
      port = 1883
    }
    mqtts = {
      name = "${var.prefix}-private-mqtts"
      port = 8883
    }
    api = {
      name = "${var.prefix}-private-api"
      port = 18083
    }
  }
}

data "aws_route53_zone" "public" {
  name = var.public_domain_name
}

module "vpc" {
  source     = "./modules/vpc"
  prefix     = var.prefix
  cidr       = var.vpc_cidr
  vpc_region = var.region
}

resource "aws_lb_target_group" "emqx_public" {
  for_each = local.emqx_public_target_groups

  name                              = each.value.name
  port                              = each.value.port
  protocol                          = "TCP"
  target_type                       = "instance"
  vpc_id                            = module.vpc.vpc_id
  load_balancing_cross_zone_enabled = true
  connection_termination            = true
  deregistration_delay              = 0

  health_check {
    interval            = 30
    port                = each.value.port
    protocol            = "TCP"
    healthy_threshold   = 3
    unhealthy_threshold = 3
  }
}

module "public_nlb" {
  source          = "./modules/public_nlb"
  prefix          = var.prefix
  vpc_id          = module.vpc.vpc_id
  subnet_ids      = module.vpc.public_subnet_ids
  route53_zone_id = data.aws_route53_zone.public.id
  domain_name     = local.public_lb_domain_name

  dashboard_target_group_arn = aws_lb_target_group.emqx_public["dashboard"].arn
  mqtts_target_group_arn     = aws_lb_target_group.emqx_public["mqtts"].arn
  wss_target_group_arn       = aws_lb_target_group.emqx_public["wss"].arn
}

resource "aws_lb_target_group" "emqx_private" {
  for_each = local.emqx_private_target_groups

  name                              = each.value.name
  port                              = each.value.port
  protocol                          = "TCP"
  target_type                       = "instance"
  vpc_id                            = module.vpc.vpc_id
  load_balancing_cross_zone_enabled = true
  connection_termination            = true
  deregistration_delay              = 0

  health_check {
    interval            = 30
    port                = each.value.port
    protocol            = "TCP"
    healthy_threshold   = 3
    unhealthy_threshold = 3
  }
}

module "internal_nlb" {
  source     = "./modules/internal_nlb"
  prefix     = var.prefix
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnet_ids

  mqtt_target_group_arn  = aws_lb_target_group.emqx_private["mqtt"].arn
  mqtts_target_group_arn = aws_lb_target_group.emqx_private["mqtts"].arn
  api_target_group_arn   = aws_lb_target_group.emqx_private["api"].arn
}

module "certs" {
  source = "./modules/certs"
  subject = {
    cn = "EMQX"
    o  = "EMQ Technologies"
    c  = "SE"
  }
}

resource "aws_route53_zone" "vpc" {
  name = local.route53_zone_name
  vpc {
    vpc_id     = module.vpc.vpc_id
    vpc_region = var.region
  }
  tags = {
    Name = var.prefix
  }
}

resource "aws_security_group_rule" "allow_traffic_from_nlb" {
  for_each = {
    allow_traffic_from_public_nlb = {source_security_group_id = module.public_nlb.security_group_id},
    allow_traffic_from_internal_nlb = {source_security_group_id = module.internal_nlb.security_group_id},
  }
  type                     = "ingress"
  security_group_id        = module.vpc.security_group_id
  source_security_group_id = each.value.source_security_group_id
  protocol                 = "-1"
  from_port                = 0
  to_port                  = 0
}

module "emqx_core_asg" {
  count             = 2
  source            = "./modules/asg"
  name              = "${var.prefix}-emqx-core${count.index}"
  register_hostname = true
  hostname          = "emqx-core-${count.index}.${local.route53_zone_name}"
  instance_type     = "t3.large"
  ami_filter        = var.ami_filter
  ami_owner         = var.ami_owner
  route53_zone_id   = aws_route53_zone.vpc.zone_id
  certs             = module.certs.certs

  vpc_id               = module.vpc.vpc_id
  subnet_ids           = [module.vpc.private_subnet_ids[count.index]]
  security_group_id    = module.vpc.security_group_id
  iam_instance_profile = module.vpc.aws_iam_instance_profile

  lb_target_group_arns = concat(
    [for tg in aws_lb_target_group.emqx_public : tg.arn],
    [for tg in aws_lb_target_group.emqx_private : tg.arn],
  )

  min_size         = 1
  max_size         = 1
  desired_capacity = 1

  extra_user_data = <<-EOF
    curl -s https://packagecloud.io/install/repositories/emqx/emqx-enterprise5/script.deb.sh | bash
    apt-get install -y emqx-enterprise
    systemctl stop emqx
    echo "node.name = \"emqx@$(hostname -f)\"" >> /etc/emqx/emqx.conf
    echo "cluster.discovery_strategy = static" >> /etc/emqx/emqx.conf
    echo "cluster.static.seeds = [\"${local.static_seeds[0]}\", \"${local.static_seeds[1]}\"]" >> /etc/emqx/emqx.conf
    echo "dashboard.default_password = admin" >> /etc/emqx/emqx.conf
    systemctl start emqx
    systemctl enable emqx
  EOF
}
