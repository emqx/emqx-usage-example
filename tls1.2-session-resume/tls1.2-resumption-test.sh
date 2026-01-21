#!/bin/bash

# Test TLS 1.2 session resumption on a single server
# This script:
# 1. Connects to server using TLS 1.2
# 2. Verifies TLS 1.2 session ID was received
# 3. Disconnects
# 4. Reconnects to the same server using the session ID
# 5. Verifies session resumption worked
# 6. Prints result in green (success) or red (failure)
#
# Requirements:
# - Server must support TLS 1.2
# - Server must support session resumption (session IDs)

set -euo pipefail

# Parse host:port arguments
TLS_HOST_PORT="${1:-localhost:8883}"
if [[ "$TLS_HOST_PORT" == *:* ]]; then
    TLS_HOST="${TLS_HOST_PORT%%:*}"
    TLS_PORT="${TLS_HOST_PORT##*:}"
else
    TLS_HOST="$TLS_HOST_PORT"
    TLS_PORT=8883
fi

# Certificate configuration
CERT_DIR="${TLSER_CERTS:-certs}"
CA_CERT="${CERT_DIR}/ca.pem"
CLIENT_CERT="${CERT_DIR}/client-cert.pem"
CLIENT_KEY="${CERT_DIR}/client-key.pem"

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
echo "TLS 1.2 Session Resumption Test"
echo "=========================================="
echo "Server: ${TLS_HOST}:${TLS_PORT}"
echo ""
echo "This test verifies that TLS 1.2 session resumption works"
echo "by connecting, disconnecting, and reconnecting to the same server."
echo ""

# First connection - establish TLS 1.2 session
echo "--- First Connection (Establish TLS 1.2 Session) ---"
# Force TLS 1.2 - exit with error if server doesn't support it
# TLS 1.2 session IDs are sent during the handshake
(echo ""; sleep 1) | timeout 10 openssl s_client \
    -connect "${TLS_HOST}:${TLS_PORT}" \
    -tls1_2 \
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

# Check if TLS 1.2 was negotiated - exit with error if not
if ! echo "$TLS_VERSION" | grep -qE "TLSv1\.2|1\.2"; then
    echo -e "${RED}ERROR: TLS 1.2 was not negotiated${NC}"
    echo ""

    # Provide specific error message based on what we detected
    if [ -n "$PROTOCOL_ERROR" ] || [ -n "$SSL_ERROR" ]; then
        echo "The server rejected the TLS 1.2 handshake attempt."
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
                    echo "SSL Error: Protocol version mismatch (server rejected TLS 1.2)"
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
        echo "Server negotiated: $TLS_VERSION (not TLS 1.2)"
        echo ""
    else
        echo "Could not determine negotiated TLS version."
        echo ""
    fi

    echo "This script requires TLS 1.2. The server may only support other TLS versions."
    echo ""
    echo "Possible reasons:"
    echo "  1. Server does not support TLS 1.2"
    echo "  2. Server is configured to use only TLS 1.3 or older versions"
    echo "  3. Server explicitly disabled TLS 1.2"
    echo ""
    echo "Connection details:"
    echo "  Server: ${TLS_HOST}:${TLS_PORT}"
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

# Display detailed TLS 1.2 information
echo "--- TLS 1.2 Connection Details ---"
echo "TLS Version: $TLS_VERSION"
if [ -n "$TLS_CIPHER" ]; then
    echo "Cipher Suite: $TLS_CIPHER"
fi
# Show the full line for debugging
echo "Full TLS info: $TLS_VERSION_LINE"

SESSION_FILE_SIZE=$(stat -f%z "$SESSION_FILE" 2>/dev/null || stat -c%s "$SESSION_FILE" 2>/dev/null || echo "0")
echo "Session file size: $SESSION_FILE_SIZE bytes"

# Function to extract session ID as decimal byte sequence
extract_session_id_decimal() {
    local session_file="$1"
    if [ ! -f "$session_file" ]; then
        return 1
    fi

    # Try to extract session ID from openssl sess_id output
    local sess_output=$(openssl sess_id -in "$session_file" -text -noout 2>/dev/null)
    if [ -z "$sess_output" ]; then
        return 1
    fi

    # Extract the Session-ID line (format: "Session-ID: XX:XX:XX:...")
    # Remove colons and spaces, convert to uppercase for hex conversion
    local session_id_hex=$(echo "$sess_output" | grep -i "Session-ID:" | sed 's/.*Session-ID:[[:space:]]*//' | tr -d ':' | tr -d ' ' | tr '[:lower:]' '[:upper:]')

    if [ -z "$session_id_hex" ] || [ "$session_id_hex" = "" ]; then
        return 1
    fi

    # Convert hex string to decimal bytes
    # Split hex string into pairs and convert each to decimal
    local result=""
    local len=${#session_id_hex}
    for ((i=0; i<len; i+=2)); do
        local hex_byte="${session_id_hex:$i:2}"
        if [ ${#hex_byte} -eq 2 ]; then
            # Convert hex to decimal using arithmetic expansion
            local dec_byte=$((0x$hex_byte))
            if [ -z "$result" ]; then
                result="$dec_byte"
            else
                result="$result,$dec_byte"
            fi
        fi
    done

    echo "$result"
    return 0
}

# Check if session ID was received
echo "Note: TLS 1.2 uses session IDs (stored in session file)"
if [ "$SESSION_FILE_SIZE" = "0" ]; then
    echo -e "  ${RED}ERROR: No TLS 1.2 session ID received from server${NC}"
    echo ""
    echo "For TLS 1.2, the server must send a session ID during the handshake."
    echo "An empty session file (0 bytes) indicates no session ID was received."
    echo ""
    echo "Possible reasons:"
    echo "  1. Server is not configured to support session resumption"
    echo "  2. Server has session resumption disabled"
    echo "  3. Server configuration issue with TLS 1.2 session resumption"
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
    echo "  Session file contains session data ($SESSION_FILE_SIZE bytes)"
    echo ""
    echo "--- Session File Contents (for inspection) ---"
    if command -v openssl >/dev/null 2>&1; then
        # Use openssl sess_id to display session information
        SESS_INFO=$(openssl sess_id -in "$SESSION_FILE" -text -noout 2>/dev/null)
        if [ -n "$SESS_INFO" ]; then
            echo "$SESS_INFO"
        else
            echo "  (Unable to parse session file - may be binary format)"
        fi
    else
        echo "  (openssl not available for inspection)"
    fi
    echo ""

    # Extract and display session ID in decimal format for Erlang comparison
    # This is printed right after session file inspection
    if command -v openssl >/dev/null 2>&1; then
        SESSION_ID_DECIMAL=$(extract_session_id_decimal "$SESSION_FILE")
        if [ -n "$SESSION_ID_DECIMAL" ]; then
            echo "--- Session ID (decimal bytes for Erlang comparison) ---"
            echo "Session ID (decimal): $SESSION_ID_DECIMAL"
            echo ""
        fi
    fi
fi
echo ""

# Wait a moment before reconnecting
sleep 1

# Second connection - resume TLS 1.2 session on the same server
echo "--- Second Connection (Resume TLS 1.2 Session) ---"
echo "Reconnecting to ${TLS_HOST}:${TLS_PORT} using the session ID..."
# Keep connection open briefly to verify resumption
(echo ""; sleep 1) | timeout 10 openssl s_client \
    -connect "${TLS_HOST}:${TLS_PORT}" \
    -tls1_2 \
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
RESUMED=$(grep -aiE "Reused|Resumed" /tmp/tls_second.log 2>/dev/null || true)
SESSION_TICKET=$(grep -aiE "Session ticket|TLS session ticket" /tmp/tls_second.log 2>/dev/null || true)

echo "--- Verification ---"
echo "First connection:  TLS $TLS_VERSION${TLS_CIPHER:+ ($TLS_CIPHER)}"
echo "Second connection: TLS $TLS_VERSION_2${TLS_CIPHER_2:+ ($TLS_CIPHER_2)}"
if [ "$TLS_VERSION" != "$TLS_VERSION_2" ] && [ "$TLS_VERSION_2" != "unknown" ]; then
    echo -e "${RED}Warning: TLS version mismatch between connections${NC}"
fi
echo ""

# Verify that TLS 1.2 session ID was received and used
echo "Checking TLS 1.2 session resumption status..."
SESSION_REUSED=false

# Check for resumption indicators in second connection log
# Look for explicit resumption messages
if grep -aqiE "Reused|Resumed" /tmp/tls_second.log 2>/dev/null; then
    SESSION_REUSED=true
fi

# Check for session ID usage (TLS 1.2 uses session IDs for resumption)
# OpenSSL may also report session ticket extension if supported
if grep -aqiE "Session ticket|TLS session ticket" /tmp/tls_second.log 2>/dev/null; then
    # Check if session was reused (look for "Reused" or "accepted")
    SESSION_STATUS=$(grep -ai "Session ticket" /tmp/tls_second.log 2>/dev/null | grep -iE "reused|accepted" | head -1 || echo "")
    if [ -n "$SESSION_STATUS" ]; then
        SESSION_REUSED=true
        echo "  Found session reuse indicator"
    fi
fi

if [ "$SESSION_REUSED" = "false" ]; then
    echo -e "${RED}ERROR: TLS 1.2 session ID was not accepted${NC}"
    echo ""
    echo "The session ID was not accepted by ${TLS_HOST}:${TLS_PORT}."
    echo ""
    echo "Possible reasons:"
    echo "  1. Server is not configured to accept session resumption"
    echo "  2. Server has session resumption disabled"
    echo "  3. Server configuration issue with TLS 1.2 session resumption"
    echo "  4. Session cache expired or cleared"
    echo ""
    echo "Debug information:"
    echo "  Server: ${TLS_HOST}:${TLS_PORT} - TLS version: $TLS_VERSION -> $TLS_VERSION_2"
    echo "  Session resumption detected: No"
    echo ""
    echo "First connection log (TLS negotiation):"
    strings /tmp/tls_first.log 2>/dev/null | grep -a "New, TLSv" | head -3
    echo ""
    echo "Second connection log (checking for resumption):"
    strings /tmp/tls_second.log 2>/dev/null | grep -a "New, TLSv" | head -5
    echo ""
    echo "Resumption indicators searched for: Reused, Resumed, Session ticket"
    strings /tmp/tls_second.log 2>/dev/null | grep -iE "reused|resumed|session.*ticket" | head -5 || echo "  None found"
    echo ""
    rm -f "$SESSION_FILE" /tmp/tls_first.log /tmp/tls_second.log
    exit 1
else
    echo "✓ TLS 1.2 session ID was accepted and session was resumed"
fi
echo ""

# Check for TLS resumption indicators in the second connection log
RESUMPTION_SUCCESS=false

if [ -n "$RESUMED" ]; then
    echo "Session resumption detected: $RESUMED"
    RESUMPTION_SUCCESS=true
elif [ -n "$SESSION_TICKET" ] && [ -n "$(echo "$SESSION_TICKET" | grep -iE "reused|accepted")" ]; then
    echo "Session reused - session was resumed"
    echo "  Details: $SESSION_TICKET"
    RESUMPTION_SUCCESS=true
fi

# If still not detected, show debug info
if [ "$RESUMPTION_SUCCESS" = "false" ]; then
    echo "Debug: Checking logs for resumption indicators..."
    echo "  RESUMED: ${RESUMED:-<not found>}"
    echo "  SESSION_TICKET: ${SESSION_TICKET:-<not found>}"
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
    echo -e "✓ TLS 1.2 Session Resumption: SUCCESS"
    echo -e "==========================================${NC}"
    echo ""
    echo "The session ID was successfully used to resume"
    echo "the TLS 1.2 session on ${TLS_HOST}:${TLS_PORT}."
    exit 0
else
    echo -e "${RED}=========================================="
    echo -e "✗ TLS 1.2 Session Resumption: FAILED"
    echo -e "==========================================${NC}"
    echo ""
    echo "The session ID was not accepted by ${TLS_HOST}:${TLS_PORT}."
    exit 1
fi
