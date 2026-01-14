#!/usr/bin/env python3
"""
Script to generate a JWT token using HMAC with specified claims.
"""

import jwt
import datetime
import sys

def generate_jwt_token():
    # Secret key for HMAC
    secret = "jwtpassfortest1"

    # Standard claims
    now = datetime.datetime.utcnow()
    payload = {
        # Standard claims
        'iat': now,  # Issued at
        'exp': now + datetime.timedelta(hours=1),  # Expires in 1 hour
        'nbf': now,  # Not before
        'iss': 'test',  # Issuer (optional standard claim)

        # Custom claim
        'sub': 'urn:test:unitid:1001010-1000010:clientid:test'
    }

    # Generate token using HS256 (HMAC-SHA256)
    token = jwt.encode(payload, secret, algorithm='HS256')

    # Ensure token is a string (not bytes)
    if isinstance(token, bytes):
        token = token.decode('utf-8')

    return token

if __name__ == '__main__':
    try:
        token = generate_jwt_token()
        print(token)
    except Exception as e:
        print(f"Error generating JWT token: {e}", file=sys.stderr)
        sys.exit(1)
