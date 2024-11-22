variable "prefix" {
  type    = string
  default = "iov-s3-demo"
}

variable "region" {
  type    = string
  default = "eu-central-1"
}

variable "vpc_cidr" {
  type    = string
  default = "10.0.0.0/16"
}

variable "ami_filter" {
  type    = string
  default = "*ubuntu/images/hvm-ssd/ubuntu-jammy-22.04-amd64-server-*"
}

variable "ami_owner" {
  type    = string
  default = "amazon"
}

variable "emqx_license_file" {
  type    = string
  default = "emqx.lic"
}

variable "emqx_core_count" {
  type    = number
  default = 2
}

variable "emqx_replicant_count" {
  type    = number
  default = 0
}

variable "public_domain_name" {
  type    = string
  default = "emqx.works"
}

variable "s3_bucket" {
  type    = string
  default = "emqx-iov-s3-demo"
}
