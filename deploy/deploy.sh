#! /bin/bash

set -Cue

if [[ $# -lt 1 ]]; then
  echo "Usage: ./deploy.sh SERVER_HOST_NAME"
  exit -1
fi

stack install
pushd ~/.local/bin/
tar cvzf kucipong.tar.gz kucipong
scp kucipong.tar.gz "$1":
rm kucipong.tar.gz
ssh -t "$1" '
  tar xvf kucipong.tar.gz && \
  tar cvzf kucipong.keter config/keter.yaml kucipong && \
  cp kucipong.keter /opt/keter/incoming
'
popd
