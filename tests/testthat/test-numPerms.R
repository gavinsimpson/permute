library("testthat")
library("permute")

context("Testing numPerms()")

test_that("numPerms works with complex, but small, design", {
    h <- how(within = Within(type = "series", constant = TRUE),
             plots = Plots(strata = gl(2, 5), type = "series", mirror = TRUE))
    np <- numPerms(10, control = h)
    expect_is(np, "numeric")
    expect_equal(as.numeric(np), 10)
})
