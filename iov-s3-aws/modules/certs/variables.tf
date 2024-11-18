variable "subject" {
  description = "The subject of the certificate"
  type = object({
    cn = string
    o  = string
    c  = string
  })
  default = {
    cn = "EMQX"
    o  = "EMQ Technologies"
    c  = "SE"
  }
}
