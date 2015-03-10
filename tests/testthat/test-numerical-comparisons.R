test_that("relational operators within tolerance", {
  x=.5-.3
  y=.3-.1
  #expect_that(x == y, equals(FALSE)) # not always FALSE apparently
  expect_that(x %==% y, equals(TRUE))
  expect_that(x %!=% y, equals(FALSE))

  set.seed(123)
  a = jitter(1:10)
  b = jitter(1:10)
  less = a%<=%b
  greater = a%>=%b
  equal = a%==%b
  notequal = a%!=%b
  expect_that(less, equals(!greater))
  expect_that(notequal, equals(!equal))
})
