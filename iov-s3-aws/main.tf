terraform {
  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "5.79.0"
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
  emqx_public_target_groups = {
    dashboard = {
      name = "${var.prefix}-public-dashboard"
      port = 18083
    }
    mqtts = {
      name = "${var.prefix}-public-mqtts"
      port = 8883
    }
  }

  emqx_private_target_groups = {
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
  source     = "./modules/public_nlb"
  prefix     = var.prefix
  vpc_id     = module.vpc.vpc_id
  subnet_ids = module.vpc.public_subnet_ids

  dashboard_target_group_arn = aws_lb_target_group.emqx_public["dashboard"].arn
  mqtts_target_group_arn     = aws_lb_target_group.emqx_public["mqtts"].arn
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

  mqtts_target_group_arn = aws_lb_target_group.emqx_private["mqtts"].arn
  api_target_group_arn   = aws_lb_target_group.emqx_private["api"].arn
}

module "certs" {
  source = "./modules/certs"
  subject = {
    cn        = module.public_nlb.dns_name
    client_cn = "v2-demo123"
    o         = "EMQ Technologies"
    c         = "SE"
  }
}

resource "aws_route53_zone" "vpc" {
  name = local.route53_zone_name
  vpc {
    vpc_id     = module.vpc.vpc_id
    vpc_region = var.region
  }
  force_destroy = true
  tags = {
    Name = var.prefix
  }
}

resource "aws_security_group_rule" "allow_traffic_from_nlb" {
  for_each = {
    allow_traffic_from_public_nlb   = { source_security_group_id = module.public_nlb.security_group_id },
    allow_traffic_from_internal_nlb = { source_security_group_id = module.internal_nlb.security_group_id },
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
  instance_type     = "c8g.large"
  ami_filter        = "*ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-arm64-server-*"
  ami_owner         = "amazon"
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
    echo '{deny, all}.' > /etc/emqx/acl.conf
    echo "node.name = \"emqx@$(hostname -f)\"" >> /etc/emqx/emqx.conf
    echo "include \"/etc/emqx/extra.conf\"" >> /etc/emqx/emqx.conf
    aws s3 cp s3://${var.s3_bucket}/emqx/extra.conf /etc/emqx/extra.conf
    cp /etc/ssl/certs/emqx/*.pem /etc/emqx/certs/
    systemctl start emqx
    systemctl enable emqx
  EOF

  depends_on = [
    aws_s3_object.extra_conf,
  ]
}

module "auth-server" {
  source               = "./modules/ec2"
  prefix               = var.prefix
  vpc_id               = module.vpc.vpc_id
  region               = var.region
  ami_filter           = var.ami_filter
  ami_owner            = var.ami_owner
  instance_type        = "c7i.large"
  instance_name        = "auth-server"
  iam_instance_profile = module.vpc.aws_iam_instance_profile
  route53_zone_id      = aws_route53_zone.vpc.zone_id
  hostname             = "auth.${local.route53_zone_name}"
  subnet_id            = module.vpc.private_subnet_ids[0]
  security_group_id    = module.vpc.security_group_id
  certs                = module.certs.certs
  extra_user_data      = <<-EOF
    aws s3 sync s3://${var.s3_bucket}/auth-server /opt/auth-server
    python3 -m pip install -r /opt/auth-server/requirements.txt
    python3 -m pip install gunicorn
    cp /opt/auth-server/auth-server.service /etc/systemd/system/
    # create user for auth-server
    useradd -r auth-server
    systemctl daemon-reload
    systemctl start auth-server.service
    systemctl enable auth-server.service
    apt-get update && apt-get install -y nginx
    rm /etc/nginx/sites-enabled/default
    cp /opt/auth-server/nginx.conf /etc/nginx/conf.d/auth-server.conf
    systemctl restart nginx
    systemctl enable nginx
  EOF
  depends_on = [
    aws_s3_object.auth-server,
    aws_s3_object.auth-server-service,
  ]
}

module "loadgen" {
  source               = "./modules/ec2"
  prefix               = var.prefix
  vpc_id               = module.vpc.vpc_id
  region               = var.region
  ami_filter           = var.ami_filter
  ami_owner            = var.ami_owner
  instance_type        = "m7i.2xlarge"
  instance_name        = "loadgen"
  iam_instance_profile = module.vpc.aws_iam_instance_profile
  route53_zone_id      = aws_route53_zone.vpc.zone_id
  hostname             = "loadgen.${local.route53_zone_name}"
  subnet_id            = module.vpc.public_subnet_ids[0]
  security_group_id    = module.vpc.security_group_id
  certs                = module.certs.certs
  extra_user_data      = <<-EOF
    mkdir /opt/loadgen
    cd /opt/loadgen
    #wget https://github.com/emqx/emqttb/releases/download/v1.0.3/emqttb-1.0.3-ubuntu22.04-amd64-quic.tar.gz
    wget https://github.com/emqx/emqtt-bench/releases/download/0.4.25/emqtt-bench-0.4.25-ubuntu22.04-amd64-quic.tar.gz
    #tar zxf emqttb-1.0.3-ubuntu22.04-amd64-quic.tar.gz
    tar zxf emqtt-bench-0.4.25-ubuntu22.04-amd64-quic.tar.gz

    aws s3 cp s3://${var.s3_bucket}/loadgen/loadgen.service /etc/systemd/system/
    useradd -r loadgen
    chown -R loadgen:loadgen /opt/loadgen
    systemctl daemon-reload
  EOF
  depends_on = [
    aws_s3_object.loadgen-service,
  ]
}

resource "aws_s3_bucket" "emqx" {
  bucket = var.s3_bucket
}

resource "aws_s3_object" "extra_conf" {
  bucket = aws_s3_bucket.emqx.bucket
  key    = "emqx/extra.conf"

  content = templatefile("${path.module}/extra.conf.tpl", {
    s3_bucket   = aws_s3_bucket.emqx.bucket,
    region      = var.region,
    seeds       = join(",", [for item in local.static_seeds : "\"${item}\""]),
    auth_server = "auth.${local.route53_zone_name}",
  })
  etag = filemd5("${path.module}/extra.conf.tpl")
}

resource "aws_s3_object" "auth-server" {
  for_each = fileset("${path.module}/../iov-s3/auth-server", "*")
  bucket   = aws_s3_bucket.emqx.bucket
  key      = "auth-server/${each.value}"
  source   = "${path.module}/../iov-s3/auth-server/${each.value}"
  etag     = filemd5("${path.module}/../iov-s3/auth-server/${each.value}")
}

resource "aws_s3_object" "auth-server-service" {
  bucket = aws_s3_bucket.emqx.bucket
  key    = "auth-server/auth-server.service"
  source = "${path.module}/auth-server.service"
  etag   = filemd5("${path.module}/auth-server.service")
}

resource "aws_s3_object" "auth-server-nginx-conf" {
  bucket = aws_s3_bucket.emqx.bucket
  key    = "auth-server/nginx.conf"
  source = "${path.module}/nginx.conf"
  etag   = filemd5("${path.module}/nginx.conf")
}

resource "aws_s3_object" "loadgen-service" {
  bucket = aws_s3_bucket.emqx.bucket
  key    = "loadgen/loadgen.service"

  content = templatefile("${path.module}/loadgen.service.tpl", {
    emqx_target = module.public_nlb.dns_name,
  })
  etag = filemd5("${path.module}/loadgen.service.tpl")
}

resource "aws_s3_bucket_ownership_controls" "emqx" {
  bucket = aws_s3_bucket.emqx.bucket
  rule {
    object_ownership = "BucketOwnerPreferred"
  }
}

resource "aws_s3_bucket_acl" "emqx" {
  depends_on = [aws_s3_bucket_ownership_controls.emqx]
  bucket     = aws_s3_bucket.emqx.bucket
  acl        = "private"
}
