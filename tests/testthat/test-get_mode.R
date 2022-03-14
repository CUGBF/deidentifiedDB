test_that("Testing get_mode() !", {
  values_vec <- c("India", "USA", "USA", "UK", "USA")

  expect_equal(get_mode(values_vec), "USA")

  values_vec <- c(values_vec, "UK", "UK")

  expect_equal(get_mode(values_vec), "USA")

  values_vec <- c("India", "UK", "UK", "USA", "USA", "UK", "USA")

  expect_equal(get_mode(values_vec), "UK")

  values_vec <- c(2002, 2003, 2001)

  expect_equal(get_mode(values_vec), 2002)

  values_vec <- c(values_vec, 2003)

  expect_equal(get_mode(values_vec), 2003)

  values_vec <- c(TRUE, FALSE, TRUE)

  expect_equal(get_mode(values_vec), TRUE)
})
