import json
import os
import ssl
import urllib.request

context = ssl.create_default_context(cafile="/ca/auth-ca.pem")
url = f"https://{os.environ['TENANT']}.auth.example.com/health"
with urllib.request.urlopen(url, context=context, timeout=2) as response:
    assert json.load(response)["status"] == "ok"
