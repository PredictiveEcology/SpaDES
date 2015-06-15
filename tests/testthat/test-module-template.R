test_that("module templates work", {
  path <- file.path(tempdir(), "modules")
  dir.create(path)
  expect_true(file.exists(path))

  newModule("myModule", path, FALSE)

  mpath <- file.path(path, "myModule")

  expect_true(file.exists(mpath))
  expect_true(file.exists(file.path(mpath, "citation.bib")))
  expect_true(file.exists(file.path(mpath, "LICENSE")))
  expect_true(file.exists(file.path(mpath, "myModule.R")))
  expect_true(file.exists(file.path(mpath, "myModule.Rmd")))
  expect_true(file.exists(file.path(mpath, "README.txt")))

  suppressMessages( zipModule(name="myModule", path=path, version="0.0.2") )
  expect_true(file.exists(file.path(mpath, "myModule_0.0.2.zip")))

  unlink(path, recursive=TRUE)
})
