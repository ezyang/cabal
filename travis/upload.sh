#!/bin/sh

set -x

# Configuration
ACCOUNT=ezyang
REPO=cabal

ROOT=$(pwd)
git config --global user.name "Pushbot"
git config --global user.email "pushbot@$(hostname)"
git config --global push.default simple
cd travis
cp id_rsa $HOME/.ssh/id_rsa
chmod 0600 $HOME/.ssh/id_rsa
cd binaries
git init
git remote add origin git@github.com:ezyang/sozu-binaries.git
cp $ROOT/travis-install.sh .
cp $ROOT/travis-common.sh .
cp -R $HOME/.cabal .
cp -R $ROOT/dist-newstyle .
cp -R $ROOT/cabal-testsuite .
git add .
# Use original pull request commit if available, so that the status
# update goes to the right place
COMMIT=${TRAVIS_PULL_REQUEST_SHA:-$TRAVIS_COMMIT}
git commit -m '{"account":"'$ACCOUNT'", "repo":"'$REPO'", "commit": "'$COMMIT'", "tag":"'$TAG'"}'
git push -f origin "HEAD:$COMMIT/$TAG"
