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

variable "node_exporter_enabled_collectors" {
  type    = list(string)
  default = ["buddyinfo", "cpu", "diskstats", "ethtool", "filefd", "filesystem", "loadavg", "meminfo", "netdev", "netstat", "processes", "sockstat", "stat", "systemd", "tcpstat", "time", "uname", "vmstat"]
}

variable "deb_architecture_map" {
  type = map(any)
  default = {
    "armv6l" : "armhf",
    "armv7l" : "armhf",
    "aarch64" : "arm64",
    "x86_64" : "amd64",
    "i386" : "i386"
  }
}

variable "emqx_license_file" {
  type    = string
  default = "emqx.lic"
}

variable "emqx_core_count" {
  type    = number
  default = 1
}

variable "emqx_replicant_count" {
  type    = number
  default = 0
}
