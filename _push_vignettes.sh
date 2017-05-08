#!/bin/bash

## this script takes the travis-built vignettes and pushes them to gh_pages.
## based on https://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html

PKGDIR=$PWD

GH_REPO="@github.com/PredictiveEcology/SpaDES.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

for files in '*.tar.gz'; do
  tar xfz $files
done

mkdir ../../gh-pages
cd ../../gh-pages
git clone --depth=50 --branch=gh-pages $FULL_REPO SpaDES >/dev/null 2>&1

cd SpaDES
git checkout gh-pages >/dev/null 2>&1
git config user.name "PredictiveEcology-travis"
git config user.email "travis"

cp $PKGDIR/SpaDES/inst/doc/*.html vignettes/
git add vignettes/*
git commit -m "deployed vignettes to gh-pages" >/dev/null 2>&1
git push --quiet $FULL_REPO gh-pages >/dev/null 2>&1

cd $PKGDIR
