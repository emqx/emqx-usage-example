resource "aws_launch_template" "lt" {
  image_id      = data.aws_ami.selected.id
  instance_type = var.instance_type
  iam_instance_profile {
    name = var.iam_instance_profile
  }

  network_interfaces {
    security_groups = [var.security_group_id]
  }

  user_data = base64encode(templatefile("${path.module}/userdata.sh", {
    hosted_zone_id    = var.route53_zone_id
    register_hostname = var.register_hostname
    hostname          = var.hostname
    asg_name          = var.name
    certs             = var.certs
    extra             = var.extra_user_data
  }))
}

resource "aws_autoscaling_group" "asg" {
  name                = var.name
  desired_capacity    = var.desired_capacity
  max_size            = var.max_size
  min_size            = var.min_size
  target_group_arns   = var.lb_target_group_arns
  vpc_zone_identifier = var.subnet_ids

  launch_template {
    id      = aws_launch_template.lt.id
    version = "$Latest"
  }
}

data "aws_ami" "selected" {
  most_recent = true
  owners      = [var.ami_owner]

  filter {
    name   = "name"
    values = [var.ami_filter]
  }
}
