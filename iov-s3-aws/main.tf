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
  static_seeds = [
    for i in range(0, 2) : "emqx@emqx-core-${i}.${local.route53_zone_name}"
  ]
}

module "vpc" {
  source     = "./modules/vpc"
  prefix     = var.prefix
  cidr       = var.vpc_cidr
  vpc_region = var.region
}

module "public_nlb" {
  source     = "./modules/public_nlb"
  prefix     = var.prefix
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.public_subnet_ids
}

module "internal_nlb" {
  source     = "./modules/internal_nlb"
  prefix     = var.prefix
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.private_subnet_ids
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

resource "aws_security_group_rule" "allow_access_from_public_nlb" {
  type                     = "ingress"
  security_group_id        = module.vpc.security_group_id
  source_security_group_id = module.public_nlb.security_group_id
  protocol                 = "-1"
  from_port                = 0
  to_port                  = 0
}

resource "aws_security_group_rule" "allow_access_from_internal_nlb" {
  type                     = "ingress"
  security_group_id        = module.vpc.security_group_id
  source_security_group_id = module.internal_nlb.security_group_id
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

  lb_target_group_arns = [
    module.public_nlb.emqx_dashboard_target_group_arn,
    module.public_nlb.emqx_mqtt_target_group_arn,
    module.public_nlb.emqx_mqtts_target_group_arn,
    module.public_nlb.emqx_ws_target_group_arn,
    module.public_nlb.emqx_wss_target_group_arn,
    module.internal_nlb.mqtt_target_group_arn,
    module.internal_nlb.mqtts_target_group_arn,
    module.internal_nlb.httpapi_target_group_arn
  ]

  min_size         = 1
  max_size         = 1
  desired_capacity = 1

  extra_user_data = <<-EOF
    curl -s https://packagecloud.io/install/repositories/emqx/emqx-enterprise5/script.deb.sh | bash
    apt-get install emqx-enterprise
    systemctl stop emqx
    echo "node.name = \"emqx@$(hostname -f)\"" >> /etc/emqx/emqx.conf
    echo "cluster.discovery_strategy = static" >> /etc/emqx/emqx.conf
    echo "cluster.static.seeds = [\"${local.static_seeds[0]}\", \"${local.static_seeds[1]}\"]" >> /etc/emqx/emqx.conf
    echo "dashboard.default_password = admin" >> /etc/emqx/emqx.conf
    systemctl enable --now emqx
  EOF

  depends_on = [
    module.public_nlb,
    module.internal_nlb,
  ]
}
