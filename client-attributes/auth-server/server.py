import json
from flask import Flask, request, jsonify
from cryptography import x509
import base64

app = Flask(__name__)

@app.route('/', methods=['POST'])
def handle_post():
    try:
        post_data = request.get_data()
        data = json.loads(post_data.decode('utf-8'))

        # Decode and process the certificate
        cert = x509.load_der_x509_certificate(base64.b64decode(data['cert']))
        ou = cert.subject.get_attributes_for_oid(x509.NameOID.ORGANIZATIONAL_UNIT_NAME)[0].value

        # Compose the response
        response = {
            'result': 'allow',
            'client_attrs': {
                'key1': ou
            }
        }

        # Send the response as JSON
        print(f'post response sent: {response}')
        return jsonify(response), 200

    except Exception as e:
        print(f"Error: {str(e)}")
        return jsonify({"error": "Failed to process request"}), 500

if __name__ == '__main__':
    app.run(host='0.0.0.0', port=8000)
