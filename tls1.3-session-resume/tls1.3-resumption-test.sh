#!/bin/bash

# Test TLS 1.3 session resumption across servers
# This script:
# 1. Connects to first server using TLS 1.3
# 2. Verifies TLS 1.3 session ticket was received
# 3. Disconnects
# 4. Connects to second server using the session ticket from first server
# 5. Verifies session resumption worked (tests stateless ticket sharing)
# 6. Prints result in green (success) or red (failure)
#
# Requirements:
# - Both servers must support TLS 1.3
# - Both servers must share the same ticket seed/key
# - First server must send session tickets for TLS 1.3 resumption

set -euo pipefail

# Parse host:port arguments
# First endpoint (to get ticket from)
TLS_HOST_PORT_1="${1:-localhost:8883}"
if [[ "$TLS_HOST_PORT_1" == *:* ]]; then
    TLS_HOST_1="${TLS_HOST_PORT_1%%:*}"
    TLS_PORT_1="${TLS_HOST_PORT_1##*:}"
else
    TLS_HOST_1="$TLS_HOST_PORT_1"
    TLS_PORT_1=8883
fi

# Second endpoint (to use ticket on) - defaults to first endpoint if not provided
TLS_HOST_PORT_2="${2:-$TLS_HOST_PORT_1}"
if [[ "$TLS_HOST_PORT_2" == *:* ]]; then
    TLS_HOST_2="${TLS_HOST_PORT_2%%:*}"
    TLS_PORT_2="${TLS_HOST_PORT_2##*:}"
else
    TLS_HOST_2="$TLS_HOST_PORT_2"
    TLS_PORT_2=8883
fi

# Certificate configuration
CERT_DIR="${TLSER_CERTS:-certs}"
CA_CERT="${CERT_DIR}/ca.pem"
CLIENT_CERT="${CERT_DIR}/client-cert.pem"
CLIENT_KEY="${CERT_DIR}/client-key.pem"

# Delay between ticket capture and ticket reuse.
# Increase this above server ticket lifetime (e.g. >10s) to test expiry.
TLS_RESUME_SLEEP_SECONDS="${TLS_RESUME_SLEEP_SECONDS:-1}"

# Check if certificates exist
if [ ! -f "$CA_CERT" ]; then
    echo "Warning: CA certificate not found at $CA_CERT (certificate verification may fail)"
fi

# Build certificate options if client cert/key exist
CLIENT_OPTS=""
if [ -f "$CLIENT_CERT" ] && [ -f "$CLIENT_KEY" ]; then
    CLIENT_OPTS="-cert $CLIENT_CERT -key $CLIENT_KEY"
    # Use -cert_chain to send the full certificate chain (including intermediates)
    # This is needed when client-cert.pem contains both the client cert and intermediate CA
    CLIENT_OPTS="$CLIENT_OPTS -cert_chain $CLIENT_CERT"
    if [ -f "$CA_CERT" ]; then
        CLIENT_OPTS="$CLIENT_OPTS -CAfile $CA_CERT"
    fi
elif [ -f "$CA_CERT" ]; then
    # Only CA cert available (for servers that don't require client certs)
    CLIENT_OPTS="-CAfile $CA_CERT"
fi

# Color codes
GREEN='\033[0;32m'
RED='\033[0;31m'
NC='\033[0m' # No Color

SESSION_FILE=$(mktemp)

echo "=========================================="
echo "TLS 1.3 Cross-Server Session Resumption Test"
echo "=========================================="
echo "First Server (get ticket):  ${TLS_HOST_1}:${TLS_PORT_1}"
echo "Second Server (use ticket): ${TLS_HOST_2}:${TLS_PORT_2}"
echo "Delay before reuse:          ${TLS_RESUME_SLEEP_SECONDS}s"
echo ""
echo "This test verifies that stateless TLS 1.3 tickets can be"
echo "shared across servers that use the same ticket seed/key."
echo ""

# First connection - establish TLS 1.3 session on first server
echo "--- First Connection (Establish TLS 1.3 Session on ${TLS_HOST_1}:${TLS_PORT_1}) ---"
# Force TLS 1.3 - exit with error if server doesn't support it
# Keep connection open briefly to receive session ticket (TLS 1.3 tickets are sent after handshake)
(echo ""; sleep 1) | timeout 10 openssl s_client \
    -connect "${TLS_HOST_1}:${TLS_PORT_1}" \
    -tls1_3 \
    $CLIENT_OPTS \
    -sess_out "$SESSION_FILE" \
    > /tmp/tls_first.log 2>&1 || true

# Check for TLS handshake errors - look for protocol version errors
PROTOCOL_ERROR=$(strings /tmp/tls_first.log 2>/dev/null | grep -iE "protocol version|tlsv1 alert|alert protocol" | head -1 || echo "")
HANDSHAKE_FAILED=$(strings /tmp/tls_first.log 2>/dev/null | grep -iE "New, \(NONE\)|Cipher is \(NONE\)" | head -1 || echo "")
SSL_ERROR=$(strings /tmp/tls_first.log 2>/dev/null | grep -iE "^[0-9]+:error:" | head -1 || echo "")

# Extract TLS version and session info - handle binary data
# Try multiple extraction methods for robustness
TLS_VERSION_LINE=$(grep -a "New, TLSv" /tmp/tls_first.log 2>/dev/null | head -1 || echo "")
if [ -z "$TLS_VERSION_LINE" ]; then
    # Fallback: use strings to extract text from binary file
    TLS_VERSION_LINE=$(strings /tmp/tls_first.log 2>/dev/null | grep "New, TLSv" | head -1 || echo "")
fi

# Extract version number (handles TLSv1.2, TLSv1.3, etc.)
if [ -n "$TLS_VERSION_LINE" ]; then
    TLS_VERSION=$(echo "$TLS_VERSION_LINE" | grep -oE "TLSv[0-9.]+" | head -1 || echo "unknown")
    # Also extract cipher info if available
    TLS_CIPHER=$(echo "$TLS_VERSION_LINE" | grep -oE "Cipher is [^ ]+" | sed 's/Cipher is //' || echo "")
else
    TLS_VERSION="unknown"
    TLS_CIPHER=""
fi

# Check if TLS 1.3 was negotiated - exit with error if not
if ! echo "$TLS_VERSION" | grep -qE "TLSv1\.3|1\.3"; then
    echo -e "${RED}ERROR: TLS 1.3 was not negotiated${NC}"
    echo ""

    # Provide specific error message based on what we detected
    if [ -n "$PROTOCOL_ERROR" ] || [ -n "$SSL_ERROR" ]; then
        echo "The server rejected the TLS 1.3 handshake attempt."
        if [ -n "$SSL_ERROR" ]; then
            # Extract the meaningful error message
            REASON=$(echo "$SSL_ERROR" | grep -oE "tlsv1 alert [^:]+" | head -1 || echo "")
            if [ -n "$REASON" ]; then
                echo "SSL Error: $REASON"
            else
                CLEAN_ERROR=$(echo "$SSL_ERROR" | sed -E 's/^[0-9]+:error:[0-9A-F]+:SSL routines://' | sed -E 's/:.*$//' | head -1)
                if [ -n "$CLEAN_ERROR" ] && [ "$CLEAN_ERROR" != "SSL routines" ]; then
                    echo "SSL Error: $CLEAN_ERROR"
                else
                    echo "SSL Error: Protocol version mismatch (server rejected TLS 1.3)"
                fi
            fi
        elif [ -n "$PROTOCOL_ERROR" ]; then
            echo "Error: $PROTOCOL_ERROR"
        fi
        echo ""
    elif [ -n "$HANDSHAKE_FAILED" ]; then
        echo "TLS handshake failed - no cipher suite was negotiated."
        echo ""
    elif [ "$TLS_VERSION" != "unknown" ]; then
        echo "Server negotiated: $TLS_VERSION (not TLS 1.3)"
        echo ""
    else
        echo "Could not determine negotiated TLS version."
        echo ""
    fi

    echo "This script requires TLS 1.3. The server may only support older TLS versions."
    echo ""
    echo "Possible reasons:"
    echo "  1. Server does not support TLS 1.3"
    echo "  2. Server is configured to use only TLS 1.2 or older"
    echo "  3. Server explicitly disabled TLS 1.3"
    echo ""
    echo "Connection details:"
    echo "  Server: ${TLS_HOST_1}:${TLS_PORT_1}"
    if [ -n "$TLS_VERSION_LINE" ]; then
        echo "  Negotiated: $TLS_VERSION_LINE"
    else
        echo "  Handshake status: Failed"
        if [ -n "$SSL_ERROR" ]; then
            REASON=$(echo "$SSL_ERROR" | grep -oE "tlsv1 alert [^:]+" | head -1 || echo "")
            if [ -n "$REASON" ]; then
                echo "  Error: $REASON"
            else
                CLEAN_ERROR=$(echo "$SSL_ERROR" | sed -E 's/^[0-9]+:error:[0-9A-F]+:SSL routines://' | sed -E 's/:.*$//' | head -1)
                if [ -n "$CLEAN_ERROR" ] && [ "$CLEAN_ERROR" != "SSL routines" ]; then
                    echo "  Error: $CLEAN_ERROR"
                else
                    echo "  Error: Protocol version mismatch"
                fi
            fi
        elif [ -n "$PROTOCOL_ERROR" ]; then
            echo "  Error: $PROTOCOL_ERROR"
        fi
    fi
    echo ""
    echo "Full connection log (last 15 lines):"
    strings /tmp/tls_first.log 2>/dev/null | tail -15
    echo ""
    rm -f "$SESSION_FILE" /tmp/tls_first.log /tmp/tls_second.log
    exit 1
fi

# Display detailed TLS 1.3 information
echo "--- TLS 1.3 Connection Details ---"
echo "TLS Version: $TLS_VERSION"
if [ -n "$TLS_CIPHER" ]; then
    echo "Cipher Suite: $TLS_CIPHER"
fi
# Show the full line for debugging
echo "Full TLS info: $TLS_VERSION_LINE"

SESSION_FILE_SIZE=$(stat -f%z "$SESSION_FILE" 2>/dev/null || stat -c%s "$SESSION_FILE" 2>/dev/null || echo "0")
echo "Session file size: $SESSION_FILE_SIZE bytes"

# Check if session ticket was received
echo "Note: TLS 1.3 uses session tickets (stored in session file)"
if [ "$SESSION_FILE_SIZE" = "0" ]; then
    echo -e "  ${RED}ERROR: No TLS 1.3 session ticket received from server${NC}"
    echo ""
    echo "For TLS 1.3, the server must send a NewSessionTicket message after the handshake."
    echo "An empty session file (0 bytes) indicates no ticket was received."
    echo ""
    echo "Possible reasons:"
    echo "  1. Server is not configured to send session tickets"
    echo "  2. Server has session tickets disabled"
    echo "  3. Server configuration issue with TLS 1.3 session resumption"
    echo ""
    echo "Debug information:"
    echo "  TLS version: $TLS_VERSION"
    echo "  Session file size: $SESSION_FILE_SIZE bytes"
    echo ""
    echo "First connection log (TLS negotiation):"
    strings /tmp/tls_first.log 2>/dev/null | grep -a "New, TLSv" | head -3
    echo ""
    rm -f "$SESSION_FILE" /tmp/tls_first.log /tmp/tls_second.log
    exit 1
else
    echo "  Session file contains ticket data ($SESSION_FILE_SIZE bytes)"
fi
echo ""

# Wait a moment before connecting to second server
sleep "$TLS_RESUME_SLEEP_SECONDS"

# Second connection - resume TLS 1.3 session on second server using ticket from first server
echo "--- Second Connection (Resume TLS 1.3 Session on ${TLS_HOST_2}:${TLS_PORT_2}) ---"
echo "Using session ticket from ${TLS_HOST_1}:${TLS_PORT_1} to resume on ${TLS_HOST_2}:${TLS_PORT_2}..."
# Keep connection open briefly to verify resumption
(echo ""; sleep 1) | timeout 10 openssl s_client \
    -connect "${TLS_HOST_2}:${TLS_PORT_2}" \
    -tls1_3 \
    $CLIENT_OPTS \
    -sess_in "$SESSION_FILE" \
    > /tmp/tls_second.log 2>&1 || true

# Extract TLS version from second connection for comparison
TLS_VERSION_LINE_2=$(grep -a "New, TLSv" /tmp/tls_second.log 2>/dev/null | head -1 || echo "")
if [ -z "$TLS_VERSION_LINE_2" ]; then
    TLS_VERSION_LINE_2=$(strings /tmp/tls_second.log 2>/dev/null | grep "New, TLSv" | head -1 || echo "")
fi
if [ -n "$TLS_VERSION_LINE_2" ]; then
    TLS_VERSION_2=$(echo "$TLS_VERSION_LINE_2" | grep -oE "TLSv[0-9.]+" | head -1 || echo "unknown")
    TLS_CIPHER_2=$(echo "$TLS_VERSION_LINE_2" | grep -oE "Cipher is [^ ]+" | sed 's/Cipher is //' || echo "")
else
    TLS_VERSION_2="unknown"
    TLS_CIPHER_2=""
fi

# Check for TLS resumption indicators in the second connection log
# For OpenSSL s_client, a resumed session is explicitly shown as:
#   Reused, TLSv1.3, Cipher is ...
RESUMED=$(strings /tmp/tls_second.log 2>/dev/null | grep -aE "^[[:space:]]*Reused, TLSv" | head -1 || true)

echo "--- Verification ---"
echo "First connection (${TLS_HOST_1}:${TLS_PORT_1}):  TLS $TLS_VERSION${TLS_CIPHER:+ ($TLS_CIPHER)}"
echo "Second connection (${TLS_HOST_2}:${TLS_PORT_2}): TLS $TLS_VERSION_2${TLS_CIPHER_2:+ ($TLS_CIPHER_2)}"
if [ "$TLS_VERSION" != "$TLS_VERSION_2" ] && [ "$TLS_VERSION_2" != "unknown" ]; then
    echo -e "${RED}Warning: TLS version mismatch between connections${NC}"
fi
echo ""

# Verify that TLS 1.3 session ticket was received and used
echo "Checking TLS 1.3 session ticket status..."
TICKET_USED=false
if [ -n "$RESUMED" ]; then
    TICKET_USED=true
fi

if [ "$TICKET_USED" = "false" ]; then
    echo -e "${RED}ERROR: TLS 1.3 session ticket was not accepted by second server${NC}"
    echo ""
    echo "The ticket from ${TLS_HOST_1}:${TLS_PORT_1} was not accepted by ${TLS_HOST_2}:${TLS_PORT_2}."
    echo "This indicates the servers may not share the same ticket seed/key."
    echo ""
    echo "Possible reasons:"
    echo "  1. Servers do not share the same ticket seed/key"
    echo "  2. Second server is not configured to accept session tickets"
    echo "  3. Second server has session tickets disabled"
    echo "  4. Server configuration issue with TLS 1.3 session resumption"
    echo ""
    echo "Debug information:"
    echo "  First server: ${TLS_HOST_1}:${TLS_PORT_1} - TLS version: $TLS_VERSION"
    echo "  Second server: ${TLS_HOST_2}:${TLS_PORT_2} - TLS version: $TLS_VERSION_2"
    echo "  Session resumption detected: No"
    echo ""
    echo "First connection log (TLS negotiation):"
    strings /tmp/tls_first.log 2>/dev/null | grep -a "New, TLSv" | head -3
    echo ""
    echo "Second connection log (checking for resumption):"
    strings /tmp/tls_second.log 2>/dev/null | grep -a "New, TLSv" | head -5
    echo ""
    echo "Resumption indicator searched for: Reused, TLSv..."
    strings /tmp/tls_second.log 2>/dev/null | grep -aE "Reused, TLSv|New, TLSv|Resumption PSK|Max Early Data" | head -10 || echo "  None found"
    echo ""
    rm -f "$SESSION_FILE" /tmp/tls_first.log /tmp/tls_second.log
    exit 1
else
    echo "✓ TLS 1.3 session ticket from ${TLS_HOST_1}:${TLS_PORT_1} was accepted by ${TLS_HOST_2}:${TLS_PORT_2}"
    echo "  This confirms the servers share the same ticket seed/key for stateless resumption."
fi
echo ""

# Check for TLS resumption indicators in the second connection log
RESUMPTION_SUCCESS=false

if [ -n "$RESUMED" ]; then
    echo "Session resumption detected: $RESUMED"
    RESUMPTION_SUCCESS=true
fi

# If still not detected, show debug info
if [ "$RESUMPTION_SUCCESS" = "false" ]; then
    echo "Debug: Checking logs for resumption indicators..."
    echo "  RESUMED: ${RESUMED:-<not found>}"
    echo "  Expected indicator: Reused, TLSv..."
    echo "  Found handshake line:"
    strings /tmp/tls_second.log 2>/dev/null | grep -aE "Reused, TLSv|New, TLSv" | head -1 || echo "    <not found>"
    echo ""
    echo "Second connection log (last 30 lines):"
    tail -30 /tmp/tls_second.log 2>/dev/null || echo "Log file not found or empty"
fi

# Cleanup (only if successful, keep logs on failure for debugging)
if [ "$RESUMPTION_SUCCESS" = "true" ]; then
    rm -f "$SESSION_FILE" /tmp/tls_first.log /tmp/tls_second.log
fi

# Print result
echo ""
if [ "$RESUMPTION_SUCCESS" = "true" ]; then
    echo -e "${GREEN}=========================================="
    echo -e "✓ TLS 1.3 Cross-Server Session Resumption: SUCCESS"
    echo -e "==========================================${NC}"
    echo ""
    echo "The ticket from ${TLS_HOST_1}:${TLS_PORT_1} was successfully"
    echo "used to resume a session on ${TLS_HOST_2}:${TLS_PORT_2}."
    echo "This confirms stateless ticket sharing is working correctly."
    exit 0
else
    echo -e "${RED}=========================================="
    echo -e "✗ TLS 1.3 Cross-Server Session Resumption: FAILED"
    echo -e "==========================================${NC}"
    echo ""
    echo "The ticket from ${TLS_HOST_1}:${TLS_PORT_1} was not accepted"
    echo "by ${TLS_HOST_2}:${TLS_PORT_2}. The servers may not share"
    echo "the same ticket seed/key."
    exit 1
fi
