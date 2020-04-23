context("Test the calculation of numbers-at-age and proportions-at-age")

at_age <- tribble(
  ~year, ~age, ~sample_id, ~survey_abbrev,
  2018,    2,           1,          NA,
  2018,    3,           1,          NA,
  2018,    3,           1,          NA,

  2018,    4,           2,          NA,
  2018,    6,           2,          NA,
  2018,    8,           2,          NA,

  2019,    6,           3,          NA,
  2019,    7,           3,          NA,
  2019,    8,           3,          NA,

  2019,    9,           4,          NA,
  2019,    9,           4,          NA,

  2019,    9,           5,          NA,
  2019,   12,           5,          NA,
  2019,   13,           5,          NA,
  2019,   13,           5,          NA,

  2016,    4,           6,      "SYN HS",
  2016,    5,           6,      "SYN HS",
  2016,    6,           6,      "SYN HS",
  2016,    8,           6,      "SYN HS",
  2016,   20,           6,      "SYN HS",

  2019,    8,           7,      "SYN HS",
  2019,    8,           7,      "SYN HS",
  2019,   20,           7,      "SYN HS",
  2019,   20,           7,      "SYN HS",
  2019,   20,           7,      "SYN HS",

  2017,    1,           8,      "SYN QCS",
  2017,    1,           8,      "SYN QCS",
  2017,    5,           8,      "SYN QCS",
  2017,    5,           8,      "SYN QCS",
  2017,   18,           8,      "SYN QCS",
  2017,   20,           8,      "SYN QCS",

  2019,    3,           9,      "SYN QCS",
  2019,   15,           9,      "SYN QCS",
  2019,   15,           9,      "SYN QCS",
  2019,   19,           9,      "SYN QCS")
# at_age <- at_age %>%
#   mutate(survey_abbrev = as.character(survey_abbrev))

catch <- tribble(
  ~year, ~ct,
   1954, 0.533,
   1955, 1.52 ,
   1956, 2.50 ,
   1957, 0.578,
   1958, 0.398,
   1959, 0.851,
   1960, 1.11 ,
   1961, 2.36 ,
   1962, 1.48 ,
   1963, 0.671,
   1964, 0.837,
   1965, 0.677,
   1966, 0.686,
   1967, 1.68 ,
   1968, 0.941,
   1969, 2.02 ,
   1970, 0.330,
   1971, 0.101,
   1972, 0.292,
   1973, 0.575,
   1974, 0.364,
   1975, 0.954,
   1976, 1.31 ,
   1977, 1.57 ,
   1978, 2.33 ,
   1979, 1.82 ,
   1980, 1.45 ,
   1981, 0.945,
   1982, 0.525,
   1983, 0.324,
   1984, 0.373,
   1985, 0.764,
   1986, 0.920,
   1987, 1.21 ,
   1988, 0.379,
   1989, 0.611,
   1990, 2.64 ,
   1991, 2.30 ,
   1992, 3.64 ,
   1993, 4.09 ,
   1994,  4.10,
   1995,  3.70,
   1996,  7.87,
   1997,  5.34,
   1998,  7.10,
   1999,  7.36,
   2000,  7.86,
   2001, 10.7 ,
   2002,  7.96,
   2003,  7.46,
   2004,  8.72,
   2005, 19.2 ,
   2006,  7.09,
   2007,  6.38,
   2008,  4.91,
   2009,  4.16,
   2010,  3.21,
   2011,  7.59,
   2012,  7.51,
   2013, 10.7 ,
   2014, 13.6,
   2015, 12.4,
   2016, 12.3,
   2017, 11.0,
   2018, 10.3,
   2019,  4.44)

test_that("Tests for parameter d", {
  expect_error(calc_naa(NULL, survey_abbrev = "SYN HS", start_age = 1, plus_age = 20))
})

test_that("Tests for parameter survey_abbrev", {
  expect_error(calc_naa(at_age, survey_abbrev = 1, start_age = 1, plus_age = 20))
  expect_error(calc_naa(at_age, survey_abbrev = c("a", "b"), start_age = 1, plus_age = 20))
})

test_that("Tests for parameter start_age", {
  expect_error(calc_naa(at_age, survey_abbrev = NULL, start_age = NULL, plus_age = 20))
  expect_error(calc_naa(at_age, survey_abbrev = NULL, start_age = "a", plus_age = 20))
  expect_error(calc_naa(at_age, survey_abbrev = NULL, start_age = c(1, 2), plus_age = 20))
  expect_error(calc_naa(at_age, survey_abbrev = NULL, start_age = -1, plus_age = 20))
})

test_that("Tests for parameter start_age", {
  expect_error(calc_naa(at_age, survey_abbrev = NULL, start_age = 1, plus_age = "a"))
  expect_error(calc_naa(at_age, survey_abbrev = NULL, start_age = 1, plus_age = c(1, 2)))
})

test_that("Output is correct", {
  naa <- calc_naa(at_age, survey_abbrev = NULL, start_age = 1, plus_age = 10)
  expect_true(all(c(2016, 1, 0, 0, 0, 1, 1, 1, 0, 1, 0, 1) == naa[1,]))
  expect_true(all(c(2017, 1, 2, 0, 0, 0, 2, 0, 0, 0, 0, 2) == naa[2,]))
  expect_true(all(c(2018, 2, 0, 1, 2, 1, 0, 1, 0, 1, 0, 0) == naa[3,]))
  expect_true(all(c(2019, 5, 0, 0, 1, 0, 0, 1, 1, 3, 3, 9) == naa[4,]))

  naa <- calc_naa(at_age, survey_abbrev = "SYN QCS", start_age = 1, plus_age = 3)
  expect_true(all(c(2017, 1, 2, 0, 4) == naa[1,]))
  expect_true(all(c(2019, 1, 0, 0, 4) == naa[2,]))

  naa <- calc_naa(at_age, survey_abbrev = "SYN HS", start_age = 1, plus_age = 5)
  expect_true(all(c(2016, 1, 0, 0, 0, 1, 4) == naa[1,]))
  expect_true(all(c(2019, 1, 0, 0, 0, 0, 5) == naa[2,]))
})
