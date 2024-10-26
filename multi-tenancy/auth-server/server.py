import json
import re
from flask import Flask, request, jsonify

app = Flask(__name__)


# Define a regular expression pattern to extract the OU value
# This pattern handles potential spaces around '=' and ensures we only match the 'OU' field.
ou_pattern = re.compile(r'\bOU\s*=\s*([^,]+)')

def extract_ou(dn):
    match = ou_pattern.search(dn)
    if match:
        return match.group(1).strip()  # Return the matched OU value, stripping any extra spaces
    return None  # Return None if no OU is found

@app.route('/', methods=['POST'])
def handle_post():
    try:
        post_data = request.get_data()
        data = json.loads(post_data.decode('utf-8'))
        # Extract the 'dn' field from the POST data
        dn = data.get('dn')
        if not dn:
            return jsonify({"error": "No 'dn' field in request"}), 400

        # Extract the OU from the DN string
        ou = extract_ou(dn)
        if not ou:
            return jsonify({"error": "No 'OU' found in 'dn' field"}), 400

        # Create the response object
        acl = [{
            "permission": "allow",
            "action": "all",
            "topics": [f"{ou}/#"]
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

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8000)
