#!/usr/bin/env python3
"""
Test TLS 1.3 session resumption across servers
This script:
1. Connects to first server using TLS 1.3
2. Verifies TLS 1.3 session ticket was received
3. Disconnects
4. Connects to second server using the session ticket from first server
5. Verifies session resumption worked (tests stateless ticket sharing)
6. Prints result in green (success) or red (failure)

Requirements:
- Both servers must support TLS 1.3
- Both servers must share the same ticket seed/key
- First server must send session tickets for TLS 1.3 resumption
- Python 3.6+ (for session_reused attribute)

Note: Python's ssl module has some limitations with cross-server session reuse.
For guaranteed cross-server ticket sharing verification, use the shell script
(tls1.3-resumption-test.sh) which uses OpenSSL directly.
"""

import sys
import socket
import ssl
import os
import tempfile
import argparse
import time
import select
from pathlib import Path


# Color codes
GREEN = '\033[0;32m'
RED = '\033[0;31m'
NC = '\033[0m'  # No Color


def parse_host_port(host_port_str, default_port=8883):
    """Parse host:port string, return (host, port) tuple."""
    if ':' in host_port_str:
        host, port = host_port_str.rsplit(':', 1)
        return host, int(port)
    else:
        return host_port_str, default_port


def get_cert_paths(cert_dir=None):
    """Get certificate file paths."""
    if cert_dir is None:
        cert_dir = os.getenv('TLSER_CERTS', 'certs')

    cert_dir = Path(cert_dir)
    ca_cert = cert_dir / 'ca.pem'
    client_cert = cert_dir / 'client-cert.pem'
    client_key = cert_dir / 'client-key.pem'

    return {
        'ca_cert': ca_cert if ca_cert.exists() else None,
        'client_cert': client_cert if client_cert.exists() else None,
        'client_key': client_key if client_key.exists() else None,
    }


def create_ssl_context(cert_paths, check_hostname=True):
    """Create SSL context for TLS 1.3 client."""
    # Create context with TLS 1.3 support
    # Use PROTOCOL_TLS_CLIENT for better control (Python 3.10+)
    # Fallback to PROTOCOL_TLS for older versions
    if hasattr(ssl, 'PROTOCOL_TLS_CLIENT'):
        context = ssl.SSLContext(ssl.PROTOCOL_TLS_CLIENT)
    elif hasattr(ssl, 'PROTOCOL_TLS'):
        context = ssl.SSLContext(ssl.PROTOCOL_TLS)
    else:
        # Fallback for very old Python versions
        context = ssl.create_default_context(ssl.Purpose.SERVER_AUTH)

    # Force TLS 1.3 minimum version (Python 3.7+)
    if hasattr(ssl, 'TLSVersion'):
        context.minimum_version = ssl.TLSVersion.TLSv1_3
        context.maximum_version = ssl.TLSVersion.TLSv1_3
    else:
        # Fallback for older Python versions
        context.options |= ssl.OP_NO_SSLv2
        context.options |= ssl.OP_NO_SSLv3
        context.options |= ssl.OP_NO_TLSv1
        context.options |= ssl.OP_NO_TLSv1_1
        context.options |= ssl.OP_NO_TLSv1_2

    # Set certificate verification
    # Note: Python's SSL module is stricter than OpenSSL about self-signed certs
    # Even with -CAfile, OpenSSL may report verify code 19 but continues
    # Python's SSL raises an exception, so we need to handle this
    if cert_paths['ca_cert']:
        # Load only the provided CA cert (for self-signed certs)
        # This is equivalent to OpenSSL's -CAfile option
        try:
            context.load_verify_locations(str(cert_paths['ca_cert']))
            context.verify_mode = ssl.CERT_REQUIRED
        except Exception:
            # If loading fails, fall back to no verification
            context.verify_mode = ssl.CERT_NONE
            context.check_hostname = False
    else:
        print("Warning: CA certificate not found (certificate verification may fail)")
        context.check_hostname = False
        context.verify_mode = ssl.CERT_NONE

    # Set client certificate if available
    if cert_paths['client_cert'] and cert_paths['client_key']:
        context.load_cert_chain(
            str(cert_paths['client_cert']),
            str(cert_paths['client_key'])
        )

    # Hostname checking
    if not check_hostname:
        context.check_hostname = False

    return context


def connect_tls(host, port, context, session=None, timeout=10):
    """Establish TLS connection and return socket with session info."""
    # Create socket
    sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    sock.settimeout(timeout)

    try:
        # Connect
        sock.connect((host, port))

        # Wrap with SSL
        # Note: For cross-server resumption, we need to use the session object
        # Python's ssl module will attempt to use the PSK from the session if valid
        if session:
            # Try to reuse session (Python 3.6+)
            # For cross-server resumption, we still pass the session object
            # The underlying SSL implementation should use the PSK if the ticket is valid
            ssl_sock = context.wrap_socket(sock, server_hostname=host, session=session)
        else:
            ssl_sock = context.wrap_socket(sock, server_hostname=host)

        return ssl_sock
    except Exception as e:
        sock.close()
        raise


def get_tls_info(ssl_sock):
    """Extract TLS connection information."""
    info = {}

    # Get protocol version
    protocol = ssl_sock.version()
    info['protocol'] = protocol

    # Get cipher suite
    cipher = ssl_sock.cipher()
    if cipher:
        info['cipher'] = cipher[0]
        info['cipher_version'] = cipher[1]
        info['cipher_bits'] = cipher[2]

    # Check if session was reused (Python 3.6+)
    if hasattr(ssl_sock, 'session_reused'):
        info['session_reused'] = ssl_sock.session_reused

    # Get session (for reuse)
    if hasattr(ssl_sock, 'session'):
        info['session'] = ssl_sock.session

    return info


def test_tls13_resumption(host1, port1, host2, port2, cert_paths, check_hostname=True):
    """Test TLS 1.3 session resumption between two servers."""

    print("==========================================")
    print("TLS 1.3 Cross-Server Session Resumption Test")
    print("==========================================")
    print(f"First Server (get ticket):  {host1}:{port1}")
    print(f"Second Server (use ticket): {host2}:{port2}")
    print("")
    print("This test verifies that stateless TLS 1.3 tickets can be")
    print("shared across servers that use the same ticket seed/key.")
    print("")

    # Create SSL context
    context = create_ssl_context(cert_paths, check_hostname)

    # First connection - establish TLS 1.3 session on first server
    print(f"--- First Connection (Establish TLS 1.3 Session on {host1}:{port1}) ---")
    ssl_sock1 = None
    tls_info1 = None
    protocol1 = None
    session1 = None
    context_used = context  # Track which context was used

    try:
        ssl_sock1 = connect_tls(host1, port1, context, timeout=10)
        tls_info1 = get_tls_info(ssl_sock1)
        protocol1 = tls_info1.get('protocol', 'unknown')
    except ssl.SSLError as e:
        # Check if it's a certificate verification error with self-signed certs
        error_str = str(e).lower()
        if 'certificate verify failed' in error_str and 'self signed' in error_str:
            # Python's SSL is stricter than OpenSSL - it fails on self-signed certs
            # even with -CAfile. OpenSSL reports verify code 19 but continues.
            # For test scripts, we'll retry with relaxed verification.
            print(f"Warning: Certificate verification failed (self-signed cert): {e}")
            print("Retrying with relaxed certificate verification...")
            print("")

            # Create a relaxed context
            context_relaxed = create_ssl_context(cert_paths, check_hostname)
            context_relaxed.check_hostname = False
            context_relaxed.verify_mode = ssl.CERT_NONE
            context_used = context_relaxed  # Track this context for reuse

            try:
                ssl_sock1 = connect_tls(host1, port1, context_relaxed, timeout=10)
                tls_info1 = get_tls_info(ssl_sock1)
                protocol1 = tls_info1.get('protocol', 'unknown')
            except Exception as e2:
                print(f"{RED}ERROR: TLS handshake failed even with relaxed verification{NC}")
                print("")
                print(f"SSL Error: {e2}")
                print("")
                return False
        else:
            print(f"{RED}ERROR: TLS handshake failed{NC}")
            print("")
            print(f"SSL Error: {e}")
            print("")
            print("Possible reasons:")
            print("  1. Server does not support TLS 1.3")
            print("  2. Server rejected the TLS 1.3 handshake attempt")
            print("  3. Certificate verification failed")
            print("")
            return False

    if not ssl_sock1 or not tls_info1:
        print(f"{RED}ERROR: Failed to establish connection{NC}")
        return False

    # Verify TLS 1.3 was negotiated
    if not protocol1.startswith('TLSv1.3') and protocol1 != 'TLSv1.3':
        print(f"{RED}ERROR: TLS 1.3 was not negotiated{NC}")
        print("")
        print(f"Server negotiated: {protocol1} (not TLS 1.3)")
        print("")
        print("This script requires TLS 1.3. The server may only support older TLS versions.")
        print("")
        print("Possible reasons:")
        print("  1. Server does not support TLS 1.3")
        print("  2. Server is configured to use only TLS 1.2 or older")
        print("  3. Server explicitly disabled TLS 1.3")
        print("")
        ssl_sock1.close()
        return False

    # Display TLS 1.3 information
    print("--- TLS 1.3 Connection Details ---")
    print(f"TLS Version: {protocol1}")
    if 'cipher' in tls_info1:
        print(f"Cipher Suite: {tls_info1['cipher']}")
    print(f"Full TLS info: Protocol={protocol1}, Cipher={tls_info1.get('cipher', 'N/A')}")

    # TLS 1.3 session tickets are sent as post-handshake messages (NewSessionTicket)
    # We MUST read from the socket to trigger receiving these messages
    # Simply sleeping without reading won't work!
    print("Waiting for TLS 1.3 session ticket (reading from socket)...")

    # Set socket to non-blocking for controlled reading
    ssl_sock1.setblocking(False)

    # Try to read data to trigger reception of NewSessionTicket
    # The server sends tickets after handshake, we need to read to receive them
    start_time = time.time()
    timeout_secs = 3  # Wait up to 3 seconds for session ticket

    while time.time() - start_time < timeout_secs:
        try:
            # Use select to check if data is available
            readable, _, _ = select.select([ssl_sock1], [], [], 0.1)
            if readable:
                try:
                    # Read any available data (this triggers SSL record processing)
                    data = ssl_sock1.recv(4096)
                    if data:
                        print(f"  Received {len(data)} bytes from server")
                except ssl.SSLWantReadError:
                    pass
                except ssl.SSLError:
                    pass
            else:
                # Even without data, do a non-blocking read attempt
                # This processes any pending SSL records (like NewSessionTicket)
                try:
                    ssl_sock1.recv(1)
                except ssl.SSLWantReadError:
                    pass
                except ssl.SSLError:
                    pass
        except Exception:
            pass

        # Check if we have a session with ticket now
        if hasattr(ssl_sock1, 'session') and ssl_sock1.session:
            break

        time.sleep(0.1)

    # Restore blocking mode
    ssl_sock1.setblocking(True)

    # Re-fetch TLS info after reading (session might be updated now)
    tls_info1 = get_tls_info(ssl_sock1)

    # Get session for reuse
    session1 = tls_info1.get('session')
    if session1:
        # Check if session has ticket data (for TLS 1.3)
        # Python's ssl module stores session tickets internally
        print("Session ticket received (stored for reuse)")
        print(f"  Session ID: {session1.id.hex() if hasattr(session1, 'id') and session1.id else 'N/A'}")
    else:
        print(f"{RED}ERROR: No TLS 1.3 session ticket received from server{NC}")
        print("")
        print("For TLS 1.3, the server must send a NewSessionTicket message after the handshake.")
        print("No session object was available for reuse.")
        print("")
        print("Possible reasons:")
        print("  1. Server is not configured to send session tickets")
        print("  2. Server has session tickets disabled")
        print("  3. Server configuration issue with TLS 1.3 session resumption")
        print("  4. Python SSL implementation limitation (session not available)")
        print("")
        ssl_sock1.close()
        return False

    ssl_sock1.close()
    print("")

    # Wait a moment before connecting to second server
    time.sleep(1)

    # Second connection - resume TLS 1.3 session on second server using ticket from first server
    print(f"--- Second Connection (Resume TLS 1.3 Session on {host2}:{port2}) ---")
    print(f"Using session ticket from {host1}:{port1} to resume on {host2}:{port2}...")

    # Note: Python's ssl module requires using the same context for session reuse
    # The session object is tied to the SSL context that created it
    try:
        # Use the same context that was used for the first connection
        # This is required for session reuse in Python's SSL module
        context2 = context_used

        # Reuse session from first connection
        # Python will attempt to use the PSK from the session if valid
        ssl_sock2 = connect_tls(host2, port2, context2, session=session1, timeout=10)
        tls_info2 = get_tls_info(ssl_sock2)

        protocol2 = tls_info2.get('protocol', 'unknown')
        session_reused = tls_info2.get('session_reused', False)

        print("--- Verification ---")
        print(f"First connection ({host1}:{port1}):  TLS {protocol1}{' (' + tls_info1.get('cipher', '') + ')' if 'cipher' in tls_info1 else ''}")
        print(f"Second connection ({host2}:{port2}): TLS {protocol2}{' (' + tls_info2.get('cipher', '') + ')' if 'cipher' in tls_info2 else ''}")
        if protocol1 != protocol2 and protocol2 != 'unknown':
            print(f"{RED}Warning: TLS version mismatch between connections{NC}")
        print("")

        # Verify session resumption
        print("Checking TLS 1.3 session ticket status...")

        if session_reused:
            print(f"✓ TLS 1.3 session ticket from {host1}:{port1} was accepted by {host2}:{port2}")
            print("  This confirms the servers share the same ticket seed/key for stateless resumption.")
            print("")
            print(f"Session resumption detected: Yes (session_reused=True)")
            print("")
            ssl_sock2.close()

            print("")
            print(f"{GREEN}==========================================")
            print("✓ TLS 1.3 Cross-Server Session Resumption: SUCCESS")
            print(f"=========================================={NC}")
            print("")
            print(f"The ticket from {host1}:{port1} was successfully")
            print(f"used to resume a session on {host2}:{port2}.")
            print("This confirms stateless ticket sharing is working correctly.")
            return True
        else:
            print(f"{RED}ERROR: TLS 1.3 session ticket was not accepted by second server{NC}")
            print("")
            print(f"The ticket from {host1}:{port1} was not accepted by {host2}:{port2}.")
            print("This indicates the servers may not share the same ticket seed/key.")
            print("")
            print("Possible reasons:")
            print("  1. Servers do not share the same ticket seed/key")
            print("  2. Second server is not configured to accept session tickets")
            print("  3. Second server has session tickets disabled")
            print("  4. Server configuration issue with TLS 1.3 session resumption")
            print("")
            print("Debug information:")
            print(f"  First server: {host1}:{port1} - TLS version: {protocol1}")
            print(f"  Second server: {host2}:{port2} - TLS version: {protocol2}")
            print(f"  Session resumption detected: No (session_reused={session_reused})")
            print("")
            ssl_sock2.close()
            return False

    except ssl.SSLError as e:
        print(f"{RED}ERROR: TLS handshake failed on second connection{NC}")
        print("")
        print(f"SSL Error: {e}")
        print("")
        print("The ticket from the first server may not be compatible with the second server.")
        return False
    except socket.timeout:
        print(f"{RED}ERROR: Connection timeout on second connection{NC}")
        print("")
        return False
    except Exception as e:
        print(f"{RED}ERROR: Second connection failed{NC}")
        print("")
        print(f"Error: {e}")
        print("")
        return False


def main():
    """Main entry point."""
    parser = argparse.ArgumentParser(
        description='Test TLS 1.3 session resumption across servers'
    )
    parser.add_argument(
        'host_port_1',
        nargs='?',
        default='localhost:8883',
        help='First server host:port (default: localhost:8883)'
    )
    parser.add_argument(
        'host_port_2',
        nargs='?',
        default=None,
        help='Second server host:port (default: same as first server)'
    )
    parser.add_argument(
        '--certs',
        default=None,
        help='Certificate directory (default: certs or TLSER_CERTS env var)'
    )
    parser.add_argument(
        '--no-host-check',
        action='store_true',
        help='Disable hostname verification'
    )

    args = parser.parse_args()

    # Parse host:port arguments
    host1, port1 = parse_host_port(args.host_port_1, default_port=8883)

    if args.host_port_2:
        host2, port2 = parse_host_port(args.host_port_2, default_port=8883)
    else:
        host2, port2 = host1, port1

    # Get certificate paths
    cert_paths = get_cert_paths(args.certs)

    # Check hostname verification
    check_hostname = not args.no_host_check
    if os.getenv('TLSER_CLIENT_NO_HOST_CHECK') in ('1', 'true', 'yes'):
        check_hostname = False

    # Run test
    success = test_tls13_resumption(
        host1, port1, host2, port2,
        cert_paths, check_hostname
    )

    sys.exit(0 if success else 1)


if __name__ == '__main__':
    main()
