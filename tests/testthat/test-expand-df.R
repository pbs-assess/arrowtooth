context("Test the expansion of a data frame by a vector of values that correspond to a column.")

d <- tribble(
  ~year, ~x, ~y,
  2000L, 1, "a",
  2002L, 2, "b",
  2020L, 3, "c")
years <- 1998:2020
num_years <- length(years)

test_that("expand_df_by_col() - Tests for parameter df", {
  expect_error(expand_df_by_col(NULL, years, "year"))
  expect_error(expand_df_by_col(c(1, 2), years, "year"))
  expect_error(expand_df_by_col(tribble(), years, "year"))
})

test_that("expand_df_by_col() - Tests for parameter vals", {
  expect_error(expand_df_by_col(d, NULL, "year"))
})

test_that("expand_df_by_col() - Tests for parameter colname", {
  expect_error(expand_df_by_col(d, years, NULL))
  expect_error(expand_df_by_col(d, years, "yearx"))
  expect_error(expand_df_by_col(d, 1, "year"))
  expect_error(expand_df_by_col(d, c("a",2), "year"))
})

test_that("expand_df_by_col() - Output is correct", {
  outd <- expand_df_by_col(d, years, "year")
  expect_equal(nrow(outd), num_years)
  years <- 2018:2020
  num_years <- length(years)
  outd <- expand_df_by_col(d, years, "year")
  expect_true(nrow(outd) == 5)
  years <- 2020L
  outd <- expand_df_by_col(d, years, "year")
  expect_true(nrow(outd) == 3)
  years <- 2002L
  outd <- expand_df_by_col(d, years, "year")
  expect_true(nrow(outd) == 3)
  years <- 1900:2000
  outd <- expand_df_by_col(d, years, "year")
  expect_true(nrow(outd) == 103)
})

test_that("expand_df_by_col() - Output column order is correct", {
  d <- tribble(
    ~x, ~year, ~y,
    1, 2000L,  "a",
    2, 2002L,  "b",
    3, 2020L,  "c")
  years <- 1995:2016
  outd <- expand_df_by_col(d, years, "year")
  nm <- names(outd)
  expect_equal(nm[1], "x")
  expect_equal(nm[2], "year")
  expect_equal(nm[3], "y")
})