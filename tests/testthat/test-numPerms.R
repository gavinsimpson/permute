#library("testthat")
#library("permute")

context("Testing shuffleSet()")

test_that("numPerms works with complex, but small, design", {
    h <- how(within = Within(type = "series", constant = TRUE),
             plots = Plots(strata = gl(2, 5), type = "series", mirror = TRUE))
    np <- numPerms(10, control = h)
    expect_type(np, "double")
    expect_equal(np, 10L)
})

test_that("numPerms doesn't suffer floating point probs as issue 41", {
    y1 <- rnorm(150)
    f <- as.factor(c(
        rep(0, 30),
        rep(1, 30),
        rep(2, 30),
        rep(3, 30),
        rep(4, 30))
    )
    h <- how(
        within = Within(type = "none"),
        plots = Plots(strata = f, type = "free")
    )

    np <- numPerms(y1, control = h)
    expect_type(np, "double")
    expect_equal(np, 120L)

    np <- permute::check(y1, h, quietly = TRUE)
    expect_type(np$n, "double")
    expect_equal(np$n, 120L)
})
