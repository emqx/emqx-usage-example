variable "vpc_id" {
  type = string
}

variable "prefix" {
  type = string
}

variable "subnet_ids" {
  type = list(string)
}

variable "dashboard_target_group_arn" {
  type = string
}

variable "mqtts_target_group_arn" {
  type = string
}
