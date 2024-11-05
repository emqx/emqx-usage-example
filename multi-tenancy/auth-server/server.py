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
        username = data.get('username')
        if not username:
            return jsonify({"error": "No 'username' field in request"}), 400

        tns = data.get('tenant_namespace')
        # in this demo, we only check the existence of tns (tenant namespace)
        if not tns or tns == '':
            return jsonify({"error": "No 'tns' field in request"}), 400

        # Create the response object array
        acl = [{
            "permission": "allow",
            "action": "all",
            # The ACL rules should define permissions *without* tns prefix.
            "topics": [f"{username}/#"]
        }]
        response = {
                "result": "allow",
                "acl": acl
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
