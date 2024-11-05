import json
import re
import signal
import sys
from flask import Flask, request, jsonify

app = Flask(__name__)

@app.route('/', methods=['POST'])
def handle_post():
    try:
        post_data = request.get_data()
        data = json.loads(post_data.decode('utf-8'))
        vin = data.get('vin')
        if not vin:
            return jsonify({"error": "No 'vin' field in request"}), 400

        # Create the base ACL
        acl = [{
            "permission": "allow",
            "action": "all",
            # Allow a client to pub/sub all topics having its VIN as prefix
            "topics": [f"{vin}/#"]
        }]
        # If vin starts with 'v2-', add additional permission
        if vin.startswith('v2-'):
            acl.append({
                "permission": "allow",
                "action": "publish",
                "topics": ["#"]
            })

        response = {
                "result": "allow",
                "acl": acl,
                # also add vin to client attributes
                "client_attrs": {"vin" : vin}
        }
        # Return the response with status 200
        return jsonify(response), 200

    except Exception as e:
        print(f"Error: {str(e)}")
        return jsonify({"error": "Failed to process request"}), 500

def handle_sigterm(*args):
    print("Shutting down...")
    sys.exit(0)

if __name__ == '__main__':
    signal.signal(signal.SIGTERM, handle_sigterm)
    app.run(host='0.0.0.0', port=8000)
