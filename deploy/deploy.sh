#! /bin/bash

set -Cue

if [[ $# -lt 1 ]]; then
  echo "Usage: ./deploy.sh SERVER_HOST_NAME"
  exit -1
fi

stack install
pushd ./deploy
cp ~/.local/bin/kucipong .
tar cvzf kucipong.keter kucipong config/keter.yaml
scp kucipong.keter "$1":
rm kucipong.keter kucipong
ssh -t "$1" '
  cp kucipong.keter /opt/keter/incoming
'
popd
