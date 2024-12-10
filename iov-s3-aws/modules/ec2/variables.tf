variable "region" {
  type = string
}

variable "vpc_id" {
  type = string
}

variable "availability_zone_index" {
  type    = number
  default = 0
}

variable "prefix" {
  type = string
}

variable "ami_filter" {
  type = string
}

variable "ami_owner" {
  type = string
}

variable "instance_type" {
  type = string
}

variable "extra_user_data" {
  type    = string
  default = ""
}

variable "instance_name" {
  type = string
}

variable "iam_instance_profile" {
  type = string
}

variable "route53_zone_id" {
  type = string
}

variable "hostname" {
  type = string
}

variable "subnet_id" {
  type = string
}

variable "security_group_id" {
  type = string
}

variable "root_volume_size" {
  description = "Root volume size"
  type        = number
  default     = 20
}

variable "certs" {
  description = "Certificates"
  type = object({
    ca          = string
    server_cert = string
    server_key  = string
    client_cert = string
    client_key  = string
  })
}
