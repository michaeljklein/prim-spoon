#!/bin/bash


# set -e

# if [ "$#" -ne 1 ]; then
#   echo "Usage: scripts/hackage-docs.sh HACKAGE_USER"
#   exit 1
# fi

# user=$1

# cabal_file=$(find . -maxdepth 1 -name "*.cabal" -print -quit)
# if [ ! -f "$cabal_file" ]; then
#   echo "Run this script in the top-level package directory"
#   exit 1
# fi

# pkg=$(awk -F ":[[:space:]]*" 'tolower($1)=="name"    { print $2 }' < "$cabal_file")
# ver=$(awk -F ":[[:space:]]*" 'tolower($1)=="version" { print $2 }' < "$cabal_file")

# if [ -z "$pkg" ]; then
#   echo "Unable to determine package name"
#   exit 1
# fi

# if [ -z "$ver" ]; then
#   echo "Unable to determine package version"
#   exit 1
# fi

# echo "Detected package: $pkg-$ver"

# dir=$(mktemp -d build-docs.XXXXXX)
# trap 'rm -r "$dir"' EXIT

# /usr/local/Cellar/cabal-install/1.22.9.0/bin/cabal haddock --hoogle --hyperlink-source --html-location='/package/$pkg-$version/docs' --contents-location='/package/$pkg-$version'

#!/usr/bin/env bash
/usr/local/Cellar/cabal-install/1.22.9.0/bin/cabal configure && /usr/local/Cellar/cabal-install/1.22.9.0/bin/cabal build && /usr/local/Cellar/cabal-install/1.22.9.0/bin/cabal haddock --hyperlink-source \
                                    --html-location='/package/$pkg-$version/docs' \
                                    --contents-location='/package/$pkg'
S=$?
if [ "${S}" -eq "0" ]; then
    cd "dist/doc/html"
    DDIR="${1}-${2}-docs"
    cp -r "${1}" "${DDIR}" && tar -c -v -z --format=ustar -f "${DDIR}.tar.gz" "${DDIR}"
    CS=$?
    if [ "${CS}" -eq "0" ]; then
        echo "Uploading to Hackage…"
        curl -X PUT -H 'Content-Type: application/x-tar' -H 'Content-Encoding: gzip' --data-binary "@${DDIR}.tar.gz" "https://${3}:${4}@hackage.haskell.org/package/${1}-${2}/docs"
        exit $?
    else
        echo "Error when packaging the documentation"
        exit $CS
    fi
else
    echo "Error when trying to build the package."
    exit $S
fi
