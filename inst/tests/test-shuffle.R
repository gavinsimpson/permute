library(testthat)
library_if_available(permute)

context("Testing shuffle()")

## test no permutation
test_that("shuffle(n) returns seq_len(n) when not permuting", {
    ctrl <- permControl(within = Within(type = "none"))

    expect_that(shuffle(3, control = ctrl), is_identical_to(seq_len(3)))
    expect_that(shuffle(1, control = ctrl), is_identical_to(1L))
})

## test shuffle returns integers
test_that("shuffle() returns integers", {
    ctrl <- permControl(within = Within(type = "none"))

    expect_that(shuffle(4), is_a("integer"))
    expect_that(shuffle(100), is_a("integer"))
    expect_that(shuffle(1, control = ctrl), is_identical_to(1L))
    expect_that(shuffle(3, control = ctrl), is_identical_to(c(1L, 2L, 3L)))
})
