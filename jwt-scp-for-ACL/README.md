# JWT `scp` Claim for ACL

NOTE: Require EMQX 6.1.1-rc.3 or later.

This example demonstrates how to:
- parse an ADFS-style `scp` claim from JWT (`"read openid foobar"`),
- extract `read` with `regex_extract`,
- set a client attribute `jwt_scp=read` via `client_attrs_init`,
- authorize subscription with topic template `myorg/${client_attrs.jwt_scp}/#`.

## Overview

The flow in `configs/base.hocon` is:
1. **Client Attributes Init**: Extract `read` from JWT `scp` and set `client_attrs.jwt_scp`.
2. **JWT Authentication**: Validate token signature and standard claims.
3. **Authorization**: Apply file-based ACL with templated subscription topic:
   `myorg/${client_attrs.jwt_scp}/#`.

In this example, the JWT is sent in MQTT `password`.

`generate_jwt.py` includes:
- `scp = "read openid foobar"`

## Files

- `configs/base.hocon`: EMQX authn/authz configuration
- `configs/acl.conf`: ACL rule with templated topic
- `generate_jwt.py`: JWT generator with ADFS-style `scp`
- `start-emqx.sh`: Start EMQX and mount config files
- `sub-ok.sh`: Subscribe `myorg/read/#` (expected allowed)
- `sub-fail.sh`: Subscribe `myorg/openid/#` (expected rejected)
- `requirements.txt`: Python dependencies

## Usage

1. Install Python dependencies:
   ```bash
   pip install -r requirements.txt
   ```
2. Start EMQX:
   ```bash
   ./start-emqx.sh
   ```
3. Allowed subscription test:
   ```bash
   ./sub-ok.sh
   ```
4. Rejected subscription test:
   ```bash
   ./sub-fail.sh
   ```

## Cleanup

```bash
docker stop emqx
docker rm emqx
```
