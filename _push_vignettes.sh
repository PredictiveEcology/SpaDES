#!/bin/bash

## this script takes the travis-built vignettes and pushes them to gh_pages.
## based on https://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html

rm -rf tmp || exit 0;

GH_REPO="@github.com/PredictiveEcology/SpaDES.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
  tar xfz $files
done

git checkout --track -b gh-pages
git config user.name "PredictiveEcology-travis"
git config user.email "travis"

cd tmp
cp ../SpaDES/inst/doc/i-introduction.html vignettes/i-introduction.html
cp ../SpaDES/inst/doc/ii-modules.html vignettes/ii-modules.html
cp ../SpaDES/inst/doc/iii-plotting.html vignettes/iii-plotting.html

git commit -m "deployed vignettes to gh-pages"
git push --force --quiet $FULL_REPO master:gh-pages
