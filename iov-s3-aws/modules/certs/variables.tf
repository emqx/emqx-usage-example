variable "subject" {
  description = "The subject of the certificate"
  type = object({
    cn        = string
    client_cn = string
    o         = string
    c         = string
  })
  default = {
    cn        = "EMQX"
    client_cn = "EMQX Client"
    o         = "EMQ Technologies"
    c         = "SE"
  }
}
