cluster {
  discovery_strategy = static
  static {
    seeds = [${seeds}]
  }
}

dashboard {
  default_password = admin
}

listeners {
  ssl {
    default {
      enable = true
      ssl_options {
        verify = verify_peer
        fail_if_no_peer_cert = true
      }
    }
  }
  tcp {
    default {
      enable = false
    }
  }
  ws {
    default {
      enable = false
    }
  }
  wss {
    default {
      enable = false
    }
  }
}

authentication = [
  {
    mechanism = password_based
    backend = http
    url = "http://${auth_server}:8000"
    method = post
    headers {content-type = "application/json"}
    body {
      vin = "$${cert_common_name}"
      # username and password are not used in this demo
      username = "$${username}"
      password = "$${password}"
    }
  }
]

connectors {
  s3 {
    s3up1 {
      enable = true
      host = "s3.${region}.amazonaws.com"
      port = 443
      resource_opts {
        health_check_interval = "15s"
        start_after_created = true
        start_timeout = "5s"
      }
      transport_options {
        ssl {
          enable = true
        }
      }
    }
  }
}

actions {
  s3 {
    up1 {
      connector = s3up1
      enable = true
      parameters {
        acl = private
        aggregation {
          max_records = 10
          time_interval = "30s"
        }
        bucket = "${s3_bucket}"
        container {column_order = [], type = csv}
        headers {}
        key = "$${action}/$${node}/$${datetime.rfc3339utc}_N$${sequence}.csv"
        max_part_size = "5gb"
        min_part_size = "5mb"
        mode = aggregated
      }
      resource_opts {
        batch_size = 100
        batch_time = "10ms"
        health_check_interval = "15s"
        inflight_window = 100
        max_buffer_bytes = "256MB"
        query_mode = async
        request_ttl = "45s"
        worker_pool_size = 16
      }
    }
  }
}

rule_engine {
  ignore_sys_message = true
  rules {
    rule_s3up {
      actions = [
        "s3:up1"
      ]
      enable = true
      sql = """~
        SELECT
          *
        FROM
          "#"
        WHERE
          regex_match(client_attrs.vin, '^v2-.*')~"""
    }
  }
}
