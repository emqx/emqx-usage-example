variable "name" {
  description = "Name of the Auto Scaling Group"
  type        = string
}

variable "vpc_id" {
  description = "ID of the VPC"
  type        = string
}

variable "subnet_ids" {
  description = "List of subnet IDs for the ASG"
  type        = list(string)
}

variable "security_group_id" {
  description = "Security group ID for the instances"
  type        = string
}

variable "instance_type" {
  description = "EC2 instance type"
  type        = string
  default     = "t3.large"
}

variable "min_size" {
  description = "Minimum number of instances in the ASG"
  type        = number
  default     = 1
}

variable "max_size" {
  description = "Maximum number of instances in the ASG"
  type        = number
  default     = 3
}

variable "desired_capacity" {
  description = "Desired number of instances in the ASG"
  type        = number
  default     = 1
}

variable "ami_filter" {
  description = "Filter to find the AMI"
  type        = string
  default     = "*ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"
}

variable "ami_owner" {
  description = "Owner ID for the AMI"
  type        = string
  default     = "amazon"
}

variable "route53_zone_id" {
  description = "Route53 zone ID for DNS records"
  type        = string
}

variable "certs" {
  description = "Certificate configuration for EMQX"
  type        = map(string)
  sensitive   = true
}

variable "extra_user_data" {
  type    = string
  default = ""
}

variable "lb_target_group_arns" {
  description = "List of ARNs of the target groups for the ASG"
  type        = list(string)
}
