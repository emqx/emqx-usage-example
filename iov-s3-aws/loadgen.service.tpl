[Unit]
Description=EMQTT bench daemon
After=network.target

[Service]
User=loadgen
Group=loadgen
Restart=on-failure
WorkingDirectory=/opt/loadgen
ExecStart=/opt/loadgen/bin/emqtt_bench pub -c 50000 -i 1 -t v2-demo123/%%i -q 1 -I 150000 --min-random-wait 0 --max-random-wait 150000 --reconnect 10000 --num-retry-connect 5 -w -h ${emqx_target} --port 8883 --ssl --certfile /etc/ssl/certs/emqx/client-bundle.pem --keyfile /etc/ssl/certs/emqx/client-key.pem --keepalive 3600

[Install]
WantedBy=default.target
