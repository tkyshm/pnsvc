#!/bin/bash
set -xe

rsa_file=rsa_private.pem
openssl genrsa -out $rsa_file 2048
private_key=$(cat $rsa_file | sed -ze 's/\n/\\n/g')

json=$(cat <<__SERVICE_ACCOUNT__
{
  "type": "service_account",
  "project_id": "test-project-id",
  "private_key_id": "private_key_id",
  "private_key": "${private_key}",
  "client_email": "sample@sample.com",
  "client_id": "11111111111111111111111",
  "auth_uri": "https://example.com/oauth2/auth",
  "token_uri": "http://localhost:28080/oauth",
  "auth_provider_x509_cert_url": "https://example.com/oauth2/v1/certs",
  "client_x509_cert_url": "https://example.com/robot/v1/metadata/x509/firebase-adminsdk-hkkks%40days-infra.iam.gserviceaccount.com"
}'
__SERVICE_ACCOUNT__
)

echo "$json" >| priv/test_service_account.json

rm -f $rsa_file
