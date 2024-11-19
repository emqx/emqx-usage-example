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
  ssh_key_name      = var.prefix
  ssh_key_path      = pathexpand(format("./%s.pem", replace(local.ssh_key_name, "/", "-")))
  route53_zone_name = replace("${var.prefix}.emqx.io", "/", "-")
  srv_record_name   = "emqx-srv.${local.route53_zone_name}"
}

resource "tls_private_key" "pk" {
  algorithm = "RSA"
  rsa_bits  = 4096
}

resource "local_sensitive_file" "pem_file" {
  filename             = local.ssh_key_path
  file_permission      = "600"
  directory_permission = "700"
  content              = tls_private_key.pk.private_key_pem
}

module "vpc" {
  source     = "./modules/vpc"
  prefix     = var.prefix
  cidr       = var.vpc_cidr
  vpc_region = var.region
  public_key = tls_private_key.pk.public_key_openssh
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
  source = "./modules/asg"
  name   = "${var.prefix}-emqx-core"

  vpc_id               = module.vpc.vpc_id
  subnet_ids           = module.vpc.private_subnet_ids
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

  min_size         = var.emqx_core_count
  max_size         = var.emqx_core_count
  desired_capacity = var.emqx_core_count

  instance_type = "t3.large"
  ami_filter    = var.ami_filter
  ami_owner     = var.ami_owner

  route53_zone_id = aws_route53_zone.vpc.zone_id

  certs = module.certs.certs

  extra_user_data = <<-EOF
    mkdir -p /home/ubuntu/.ssh
    echo "${tls_private_key.pk.public_key_openssh}" > /home/ubuntu/.ssh/authorized_keys
    chmod 600 /home/ubuntu/.ssh/authorized_keys
    chown ubuntu:ubuntu /home/ubuntu/.ssh/authorized_keys

    curl -s https://assets.emqx.com/scripts/install-emqx-deb.sh | bash
    apt-get install emqx
    systemctl stop emqx
    echo "node.name = emqx@$(hostname -f)" >> /etc/emqx/emqx.conf
    echo "cluster.discovery_strategy = dns" >> /etc/emqx/emqx.conf
    echo "cluster.dns.name = ${local.srv_record_name}" >> /etc/emqx/emqx.conf
    echo "cluster.dns.record_type = srv" >> /etc/emqx/emqx.conf
    systemctl enable --now emqx
  EOF

  depends_on = [
    module.vpc,
    module.public_nlb,
    module.internal_nlb,
    module.certs
  ]
}

resource "aws_route53_record" "emqx_srv" {
  zone_id = aws_route53_zone.vpc.zone_id
  name    = local.srv_record_name
  type    = "SRV"
  ttl     = "60"
  records = [
    "10 20 1883 ${module.internal_nlb.dns_name}"
  ]
}

module "emqx_asg_event_handler" {
  source          = "./modules/emqx_asg_event_handler"
  asg_name        = module.emqx_core_asg.name
  region          = var.region
  route53_zone_id = aws_route53_zone.vpc.zone_id
  srv_record_name = local.srv_record_name
}
