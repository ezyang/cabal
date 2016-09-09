. ./common.sh
cd plain
cabal configure $CABAL_INTEGRATIONTESTS_PACKAGE_DB_FLAGS
cabal build
