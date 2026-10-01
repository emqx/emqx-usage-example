FROM python:3.12-slim

RUN apt-get update \
    && apt-get install -y --no-install-recommends openssl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /demo
COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt
COPY *.py generate-certs.sh ./
ENV PYTHONUNBUFFERED=1
