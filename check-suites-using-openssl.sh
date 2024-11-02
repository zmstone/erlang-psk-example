#!/bin/bash

set -euo pipefail

ciphers=("RSA-PSK-AES256-GCM-SHA384"
         "RSA-PSK-AES256-CBC-SHA384"
         "RSA-PSK-AES128-GCM-SHA256"
         "RSA-PSK-AES128-CBC-SHA256"
         "RSA-PSK-AES256-CBC-SHA"
         "RSA-PSK-AES128-CBC-SHA"
         "RSA-PSK-RC4-SHA"
         "PSK-AES256-GCM-SHA384"
         "PSK-AES128-GCM-SHA256"
         "PSK-AES256-CBC-SHA384"
         "PSK-AES256-CBC-SHA"
         "PSK-AES128-CBC-SHA256"
         "PSK-AES128-CBC-SHA")

PSK_ID='client'
# 'the-shared-secret' hex encoded is: '7468652d7368617265642d736563726574'
PSK_PASS='7468652d7368617265642d736563726574'

log_red() {
  local RED='\033[0;31m' # Red
  local NC='\033[0m' # No Color
  echo -e "${RED}${1}${NC}"
}

for i in "${ciphers[@]}"; do
    if openssl s_client -connect localhost:9999 -CAfile tls/ca.pem -psk_identity "$PSK_ID" -psk "$PSK_PASS" -tls1_2 -cipher "$i" </dev/null >/dev/null 2>&1; then
        echo "$i ............... OK"
    else
        log_red "$i ............... ERROR"
    fi
done
