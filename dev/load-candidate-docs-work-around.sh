# see issue https://github.com/haskell/cabal/issues/10252
# replace PASSWORD
curl -X PUT \
  --user JohnWalker:PASSWORD \
  -H "Content-Type: application/x-tar" \
  -H 'Content-Encoding: gzip' \
  --data-binary "@dist-newstyle/webdriver-precore-0.0.0.1-docs.tar.gz" \
  https://hackage.haskell.org/package/webdriver-precore-0.0.0.1/candidate/docs