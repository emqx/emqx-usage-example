#!/bin/sh

# this sceript is to be run as docker entrypoint for minio/mc
# to create a bucket named demobucket

mc config host add myminio http://s3:9000 ACCESSKEY SECRETKEY
mc rm -r --force myminio/demobucket
mc mb myminio/demobucket
mc policy set download myminio/demobucket
