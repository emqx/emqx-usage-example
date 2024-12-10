data "aws_availability_zones" "default" {
  state = "available"
}

data "aws_ami" "default" {
  most_recent = true
  owners      = [var.ami_owner]

  filter {
    name   = "name"
    values = [var.ami_filter]
  }
}

resource "aws_network_interface" "default" {
  subnet_id       = var.subnet_id
  security_groups = [var.security_group_id]
}

resource "aws_instance" "default" {
  ami                  = data.aws_ami.default.id
  instance_type        = var.instance_type
  iam_instance_profile = var.iam_instance_profile
  network_interface {
    device_index         = 0
    network_interface_id = aws_network_interface.default.id
  }
  user_data = templatefile("${path.module}/templates/user_data.tpl",
    {
      extra    = var.extra_user_data
      hostname = var.hostname
      certs    = var.certs
  })

  tags = {
    Name = var.instance_name
  }

  root_block_device {
    volume_size = var.root_volume_size
    volume_type = "gp3"
  }
}

resource "aws_route53_record" "dns" {
  zone_id = var.route53_zone_id
  name    = var.hostname
  type    = "A"
  ttl     = 30
  records = [aws_instance.default.private_ip]
}
