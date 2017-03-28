set.seed(1)
N = 1E5

test_normal = function(N, mean, sd) {
  x = rnorm(N, mean, sd)

  s = moment_stream$new()

  s$update(x)
  expect_equal(s$m, mean(x))
  expect_equal(s$v_hat, var(x))
}

context("scalars")
test_that("scalars work", {
  x = rnorm(100, 0, 1)
  s = moment_stream$new()
  for (i in 1:length(x)) {
    s$update(x[[i]])
  }
  expect_equal(s$m, mean(x))
  expect_equal(s$v_hat, var(x))
})

context("vectors")
test_that("small mean, small variance works", {
  test_normal(N, 0, .1)
})
test_that("large mean, small variance works", {
  test_normal(N, 1E9, .1)
})
test_that("small mean, large variance works", {
  test_normal(N, 0, 1E9)
})
test_that("large mean, large variance works", {
  test_normal(N, 1E9, 1E9)
})
test_that("combinations work", {
  x = rnorm(1E3, 0, 1)

  s = moment_stream$new()

  s$update(x)
  expect_equal(s$m, mean(x))
  expect_equal(s$v_hat, var(x))

  x2 = rnorm(1E3, 1E9, 1E9)
  s$update(x2)
  expect_equal(s$m, mean(c(x, x2)))
  expect_equal(s$v_hat, var(c(x, x2)))
})


context("lists")
test_that("lists of scalars work", {
  x = rnorm(1E3)
  s = moment_stream$new()
  s$update(as.list(x))

  expect_equal(s$m, mean(x))
  expect_equal(s$v_hat, var(x))
})
test_that("lists of matrices work", {
  x = replicate(10, matrix(rnorm(5E3), 5), simplify = FALSE)
  s = moment_stream$new()
  s$update(x)

  expect_equal(s$m, apply(simplify2array(x), 1:2, mean))
  expect_equal(s$v_hat, apply(simplify2array(x), 1:2, var))
})

context("multiple streams")
test_that("multiple streams work",{
  # e.g. that reference semantics don't cause interference among streams
  x1 = rnorm(1E3)
  x2 = rnorm(1E3)
  s1 = moment_stream$new(x1)
  s2 = moment_stream$new(x2)

  expect_equal(s1$m, mean(x1))
  expect_equal(s2$m, mean(x2))

  expect_equal(s1$v_hat, var(x1))
  expect_equal(s2$v_hat, var(x2))
})
