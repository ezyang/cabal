set -e

CABAL_VERSION="1.25.0.0"

# ---------------------------------------------------------------------
# Timing / diagnostic output
# ---------------------------------------------------------------------

# WARNING: This function does not handle quotes correctly.
timed() {
    echo "\$ $*"
    start_time=$(date +%s)
    $* || exit $?
    end_time=$(date +%s)
    duration=$((end_time - start_time))
    echo "$* took $duration seconds."
    echo "----"
}
