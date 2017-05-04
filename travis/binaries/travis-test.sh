#!/bin/sh

. ./travis-common.sh

# --hide-successes uses terminal control characters which mess up
# Travis's log viewer.  So just print them all!
TEST_OPTIONS=""

# Setup symlink so that paths look the same
mkdir -p $(dirname $UPSTREAM_BUILD_DIR)
ln -s $TRAVIS_BUILD_DIR $UPSTREAM_BUILD_DIR

# Run tests
(timed Cabal/unit-tests $TEST_OPTIONS) || exit $?

   if [ "x$PARSEC" = "xYES" ]; then
       # Parser unit tests
       (cd Cabal && timed ./parser-tests $TEST_OPTIONS) || exit $?

       # Test we can parse Hackage
       (cd Cabal && timed ./parser-hackage-tests $TEST_OPTIONS) | tail || exit $?
   fi

(cd cabal-testsuite && timed dist/build/cabal-tests/cabal-tests --builddir=dist -j3 $TEST_OPTIONS) || exit $?

# Redo the package tests with different versions of GHC
if [ "x$TEST_OTHER_VERSIONS" = "xYES" ]; then
    (cd cabal-testsuite && timed dist/build/cabal-tests/cabal-tests --builddir=dist $TEST_OPTIONS --with-ghc="/opt/ghc/7.0.4/bin/ghc")
    (cd cabal-testsuite && timed dist/build/cabal-tests/cabal-tests --builddir=dist $TEST_OPTIONS --with-ghc="/opt/ghc/7.2.2/bin/ghc")
    (cd cabal-testsuite && timed dist/build/cabal-tests/cabal-tests --builddir=dist $TEST_OPTIONS --with-ghc="/opt/ghc/head/bin/ghc")
fi

if [ "x$CABAL_LIB_ONLY" = "xYES" ]; then
    exit 0;
fi

# ---------------------------------------------------------------------
# cabal-install
# ---------------------------------------------------------------------

# Update index
(timed cabal-install/cabal update) || exit $?

# Run tests
(timed cabal-install/unit-tests $TEST_OPTIONS) || exit $?
(timed cabal-install/memory-usage-tests $TEST_OPTIONS) || exit $?

# These need the cabal-install directory
(cd cabal-install && timed ./solver-quickcheck  $TEST_OPTIONS --quickcheck-tests=1000) || exit $?
(cd cabal-install && timed ./integration-tests2 $TEST_OPTIONS) || exit $?

# Big tests
(cd cabal-testsuite && timed dist/build/cabal-tests/cabal-tests --builddir=dist -j3 --skip-setup-tests --with-cabal ${TRAVIS_BUILD_DIR}/cabal-install/cabal --with-hackage-repo-tool ${TRAVIS_BUILD_DIR}/hackage-repo-tool/hackage-repo-tool $TEST_OPTIONS) || exit $?
