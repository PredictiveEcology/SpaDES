#!/bin/bash

## this script takes the travis-built vignettes and pushes them to gh_pages.
## based on https://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html

GH_REPO="@github.com/PredictiveEcology/SpaDES.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
  tar xfz $files
done

git fetch origin gh-pages
git checkout gh-pages
git config user.name "PredictiveEcology-travis"
git config user.email "travis"

cp SpaDES/inst/doc/*.html vignettes/

git add vignettes/*.html
git commit -m "deployed vignettes to gh-pages"
git push --quiet $FULL_REPO gh-pages
