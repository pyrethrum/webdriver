set -e

# take API key as a parameter
# Prompt for API key if not provided as a parameter
if [ "$#" -ne 1 ]; then
  echo "This script uploads the package tarball to Hackage."
  echo "It requires the API key for Hackage."
  read -sp "Enter your Hackage API key: " API_KEY
  echo
else
  API_KEY="$1"
fi

echo "checking package..."
cabal check

echo "generating docs..."
DOCS_TARBALL=$(cabal haddock --haddock-for-hackage --enable-doc | awk '/^Documentation tarball created:/ {getline; print}')
# echo "The Docs: $DOCS_TARBALL"

PACKAGE_NAME=$(basename $DOCS_TARBALL | sed 's/-docs\.tar\.gz$//')
# echo "Package name: $PACKAGE_NAME"

echo "generating package..."
cabal sdist
SOURCE_TARBALL=$(cabal sdist | awk '/^Wrote tarball/ {getline; print}')
# echo "Tarball to upload: $SOURCE_TARBALL"

echo "Uploading source tarball..."
# echo "token is $API_KEY"
cabal upload --token=$API_KEY "$SOURCE_TARBALL"

# see issue https://github.com/haskell/cabal/issues/10252
# when fixed replace with cabal upload --username="" --password="$API_KEY" "$DOCS_TARBALL" --documentation
echo "Uploading docs tarball..."
curl -X PUT \
  --header "Authorization: X-ApiKey $API_KEY" \
  -H "Content-Type: application/x-tar" \
  -H 'Content-Encoding: gzip' \
  --data-binary "$DOCS_TARBALL" \
  https://hackage.haskell.org/package/$PACKAGE_NAME/candidate/docs

echo "Upload complete for $PACKAGE_NAME"
