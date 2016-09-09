set -x
. ./common.sh
cd plain
ghc --version
which ghc
echo "$CABAL_INTEGRATIONTESTS_PACKAGE_DB_FLAGS"
cabal configure $CABAL_INTEGRATIONTESTS_PACKAGE_DB_FLAGS
cabal build
