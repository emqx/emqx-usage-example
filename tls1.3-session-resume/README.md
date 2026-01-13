# TLS 1.3 Session Resumption Testing

This example demonstrates TLS 1.3 session resumption with EMQX using stateless session tickets.

TLS 1.3 session resumption allows clients to resume previous sessions without a full handshake, reducing latency and computational overhead. This is particularly useful for:

- IoT devices with frequent reconnections
- Mobile clients with intermittent connectivity
- High-throughput scenarios requiring fast connection establishment
- **Cross-server session resumption** in clustered deployments (when servers share the same ticket seed)

## Prerequisites

- Docker and Docker Compose
- Python 3.6+ (for Python test script)
- OpenSSL (for shell test script)

## Usage

### Start EMQX

```bash
docker compose up -d
```

Open <http://localhost:18083> to visit EMQX Dashboard (password: `admin`).

### Run Tests

#### Python Test

```bash
./tls1.3-resumption-test.py --no-host-check localhost:8883
```

#### Shell Test (OpenSSL)

```bash
./tls1.3-resumption-test.sh localhost:8883
```

Both scripts will:
1. Connect to the server using TLS 1.3
2. Receive a session ticket (NewSessionTicket)
3. Disconnect
4. Reconnect using the session ticket
5. Verify session resumption succeeded

## Configuration

### Key EMQX Settings

The `config/listeners.hocon` configures:

- **Enable TLS 1.3**: `versions = ["tlsv1.3"]`
- **Stateless session tickets**: `session_tickets = stateless`

The ticket seed for cross-server resumption is configured in:

```
node.tls_stateless_tickets_seed = "your-shared-secret-here"
```

### Cross-Server Resumption

The example starts two independent EMQX nodes (emqx1 on port 8883, emqx2 on port 8884).

**Note**: We do not cluster the EMQX nodes in this demo for simplicity. TLS 1.3 stateless session tickets require no cluster state sharing — any node with the same `tls_stateless_tickets_seed` can decrypt and validate tickets issued by another node.

To test cross-server resumption:

```bash
./tls1.3-resumption-test.sh localhost:8883 localhost:8884
```

## Certificates

The `certs/` directory contains test certificates:

- `ca.pem` - Root CA certificate
- `server-cert.pem` / `server-key.pem` - Server certificate and key
- `client-cert.pem` / `client-key.pem` - Client certificate and key (optional, for mTLS)

**Note**: Client certificates (mTLS) are not required for TLS 1.3 session resumption. The example is configured with `verify = verify_none` by default.

## How It Works

### TLS 1.3 Session Tickets

Unlike TLS 1.2, TLS 1.3 sends session tickets as **post-handshake messages** (`NewSessionTicket`). The client must:

1. Complete the TLS handshake
2. Read from the socket to receive the `NewSessionTicket` message
3. Store the ticket for future connections

### Stateless vs Stateful Tickets

- **Stateless**: Ticket contains encrypted session state; server doesn't store anything
- **Stateful**: Server stores session state; ticket is just an identifier

EMQX uses stateless tickets, which enables cross-server session resumption when all servers share the same ticket encryption key (seed).
