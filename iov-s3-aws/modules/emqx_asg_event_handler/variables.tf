variable "region" {
  description = "AWS region where resources are created."
  type        = string
}

variable "asg_name" {
  description = "Name of the Auto Scaling Group."
  type        = string
}

variable "route53_zone_id" {
  description = "Route 53 Zone ID."
  type        = string
}

variable "srv_record_name" {
  description = "SRV record name."
  type        = string
}
