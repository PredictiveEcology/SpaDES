test_that("relational operators within tolerance", {
  x=.5-.3
  y=.3-.1
  #expect_that(x == y, equals(FALSE)) # not always FALSE apparently
  expect_that(x %==% y, equals(TRUE))
  expect_that(x %!=% y, equals(FALSE))

  set.seed(123L)
  a = jitter(1:10, 1e-3)
  b = jitter(1:10, 1e-3)
  less = a%<=%b
  greater = a%>=%b
  equal = a%==%b
  notequal = a%!=%b
  expect_that(less, equals(!greater))
  expect_that(notequal, equals(!equal))

  a = jitter(1:10, 1e-7)
  b = jitter(1:10, 1e-7)
  less = a%<=%b
  greater = a%>=%b
  equal = a%==%b
  notequal = a%!=%b
  ids = c(5L,8L)
  expect_that(less[ids], equals(!greater[ids]))
  expect_that(less[-ids], equals(greater[-ids]))
  expect_that(all(!equal[ids]), equals(TRUE))
  expect_that(all(equal[-ids]), equals(TRUE))
})
