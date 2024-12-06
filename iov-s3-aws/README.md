# EMQX Enterprise For Internet of Vehicles

Collection of scripts to setup EMQX Enterprise 5.8.2 for IoV (Internet of Vehicles) demo environment in AWS.

Demoed features:

- TLS termination and mutual TLS authentication by EMQX
- Client certificate information extraction (VIN extraction).
- HTTP authentication with ACL.
- Collect messages in files and upload to s3.

## Start

Prerequisite: install [Terraform](https://developer.hashicorp.com/terraform/install).

```bash
terraform init
terraform apply
```

After the setup is complete, you will see the URL for EMQX dashboard, and endpoint to connect to MQTTS.

Dashboard login username is `admin` and password is also `admin`.

Test client certificate is created with CN `v2-demo123`.

To test the connection, authentication and integration to S3 one can use [mqttx](https://mqttx.app/downloads).

Subscribe in one terminal window:

```
mqttx sub -h iov-s3-demo-public-lb-<generated_lb_id>.elb.eu-central-1.amazonaws.com -l mqtts -p 8883 --ca certs/cacert.pem --cert certs/client-bundle.pem --key certs/client-key.pem -q 1 -t v2-demo123/1
```

and publish in another terminal window:

```
mqttx pub -h iov-s3-demo-public-lb-<generated_lb_id>.elb.eu-central-1.amazonaws.com -l mqtts -p 8883 --ca certs/cacert.pem --cert certs/client-bundle.pem --key certs/client-key.pem -q 1 -t v2-demo123/1
```

Messages are saved in `s3://emqx-iov-s3-demo/up1` by default.

![Architecture](./architecture.png)
