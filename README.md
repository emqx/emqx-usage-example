# EMQX Usage Examples

1. **[EMQX Cluster with Nginx Load Balancing](./mqtt-lb-nginx/README.md)**
   Example config for deploying an EMQX Cluster with Nginx to enable MQTT load balancing.

2. **[EMQX Cluster with HAProxy Load Balancing](./mqtt-lb-haproxy/README.md)**
   Example config for setting up an EMQX Cluster using HAProxy for MQTT load balancing.

3. **[Client Attributes-Based Authentication](./client-attributes/README.md)**
   Implementation of client attribute-based authentication for enhanced security.

4. **[Multi-Tenancy for MQTT Clients](./multi-tenancy/README.md)**
   Configuration and management of multi-tenancy support for MQTT clients.

5. **[Internet of Vehicles Data Aggregation and S3 Upload](./iov-s3/README.md)**
   Solution for aggregating vehicle data into files and uploading to Amazon S3.

6. **[TLS 1.3 Session Resumption](./tls1.3-session-resume/README.md)**
   Testing TLS 1.3 session resumption with stateless tickets for fast reconnections and cross-server session sharing.

7. **[TLS 1.2 Session Resumption](./tls1.2-session-resume/README.md)**
   Testing TLS 1.2 session resumption with session IDs for fast reconnections. Session caching is local to each node, so resumption only works when reconnecting to the same server node.

8. **[JWT Claim Sub-String Match with ClientID](./jwt-claim-sub-string-match-clientid/README.md)**
   Authentication using JWT tokens with client ID validation by matching a substring extracted from the JWT token's `sub` claim.

9. **[JWT `scp` Claim in ACL](./jwt-scp-for-ACL/README.md)**
   JWT authentication with `scp` regex validation, JWT-provided client attributes, and templated subscription authorization.
