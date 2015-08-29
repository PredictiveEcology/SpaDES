#!/bin/bash

## this script takes the travis-built vignettes and pushes them to gh_pages.
## based on https://rmflight.github.io/posts/2014/11/travis_ci_gh_pages.html

PKGDIR=$PWD

GH_REPO="@github.com/PredictiveEcology/SpaDES.git"

FULL_REPO="https://$GH_TOKEN$GH_REPO"

mkdir /gh-pages
cd /gh-pages
cp $PKGDIR/vignettes/*.Rmd .
Rscript -e 'rmarkdown::render(".", pattern="*.Rmd")'

git clone --depth=50 --branch=gh-pages $FULL_REPO SpaDES

cd SpaDES
git checkout gh-pages
git config user.name "PredictiveEcology-travis"
git config user.email "travis"

cp /gh-pages/*.html SpaDES/vignettes/
cp -r /gh-pages/*_files SpaDES/vignettes/
git add vignettes/*
git commit -m "deployed vignettes to gh-pages"
git push --quiet $FULL_REPO gh-pages

cd $PKGDIR
