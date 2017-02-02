#!/bin/sh

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
mkdir .cabal
cp -R $HOME/.cabal/packages .cabal
cp -R $HOME/.cabal/store .cabal
cp -R $HOME/.cabal/bin .cabal
rm -fv .cabal/packages/hackage.haskell.org/build-reports.log
rm -fv .cabal/packages/hackage.haskell.org/00-index*
rm -fv .cabal/packages/hackage.haskell.org/*.json
cp -R $ROOT/dist-newstyle .
git add .
git commit -m '{"account":"'$ACCOUNT'", "repo":"'$REPO'", "commit": "'$TRAVIS_COMMIT'", "tag":"'$TAG'"}'
git push -f origin "HEAD:$TRAVIS_COMMIT/$TAG"
