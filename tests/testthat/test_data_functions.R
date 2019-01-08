test_that("get_data() should return a data.frame", {
  data <- get_data()
  expect_is(data, "data.frame")
})

test_that(
  "get complete experiment data set",
  {
    data <- get_data()
    expect_equal(nrow(data), 96)
    expect_equal(ncol(data), 19)
  }
)

test_that(
  "get dataset by selecting TASK column",
  {
    data <- get_data(c("TASK"))
    expect_equal(nrow(data), 96)
    expect_equal(ncol(data), 1)
    col_names <- c("TASK")
    expect_equal(names(data), col_names)
  }
)

