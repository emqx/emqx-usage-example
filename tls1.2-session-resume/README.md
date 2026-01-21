# TLS 1.2 Session Resumption Testing

This example demonstrates TLS 1.2 session resumption with EMQX using session IDs.

TLS 1.2 session resumption allows clients to resume previous sessions without a full handshake, reducing latency and computational overhead. This is particularly useful for:

- IoT devices with frequent reconnections
- Mobile clients with intermittent connectivity
- High-throughput scenarios requiring fast connection establishment

**Note**: TLS 1.2 uses session IDs (not session tickets). The server sends a session ID during the handshake, and the client stores it for reuse.
Session caching is local to each node, so session resumption only works when reconnecting to the same server node.

## Prerequisites

- Docker and Docker Compose
- OpenSSL (for test script)

## Usage

### Start EMQX

```bash
./start.sh
```

Open <http://localhost:18083> to visit EMQX Dashboard (password: `admin`).

### Run Test

```bash
./tls1.2-resumption-test.sh localhost:8883
```

The test script will:
1. Connect to the server using TLS 1.2
2. Receive a session ID (during handshake)
3. Disconnect
4. Reconnect to the same server using the session ID
5. Verify session resumption succeeded

## Configuration

### Key EMQX Settings

The `config/listeners.hocon` configures:

- **Enable TLS 1.2**: `versions = ["tlsv1.2"]`

## Certificates

The `certs/` directory contains test certificates:

- `ca.pem` - Root CA certificate
- `server-cert.pem` / `server-key.pem` - Server certificate and key
- `client-cert.pem` / `client-key.pem` - Client certificate and key (optional, for mTLS)

**Note**: Client certificates (mTLS) are not required for TLS 1.2 session resumption. The example is configured with `verify = verify_none` by default.

## How It Works

### TLS 1.2 Session IDs

TLS 1.2 uses **session IDs** for session resumption. During the initial handshake, the server sends a session ID in the ServerHello message. The client stores this session ID and can present it in subsequent handshakes to resume the session.

### Session Caching

TLS 1.2 session caching is **local to each node**. The server maintains a local session cache that maps session IDs to session state. When a client presents a session ID, the server looks it up in its local cache. This means session resumption only works when reconnecting to the same server node that issued the session ID.

Unlike TLS 1.3 stateless tickets, TLS 1.2 session IDs require the server to maintain state, making cross-node resumption impossible without shared state.

## Inspecting Session Files

The test script uses OpenSSL's `-sess_out` option to save session information to a temporary file.
You can inspect this file to see the session ID and other session details.

### Using OpenSSL sess_id command

To inspect a session file manually, you can use:

```bash
# First, create a session file by connecting
openssl s_client -connect localhost:8883 -tls1_2 -sess_out /tmp/session.pem < /dev/null

# Then inspect it
openssl sess_id -in /tmp/session.pem -text -noout
```

This will display information such as:
- **Session ID**: The actual session ID value (hexadecimal)
- **Protocol**: TLS version (TLSv1.2)
- **Cipher**: Cipher suite used
- **Master-Key**: The master secret (if available)
- **Key-Arg**: Additional key material
- **Start Time**: When the session was created
- **Timeout**: Session timeout value

### What the session file contains

The session file created by `-sess_out` is in PEM format and contains:
- The session ID (32 bytes maximum in TLS 1.2)
- Cipher suite information
- Compression method
- Master secret (encrypted/encoded)
- Session lifetime information

**Note**: The session file is temporary and is automatically cleaned up by the test script after successful resumption.
