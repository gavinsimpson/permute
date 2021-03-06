library("testthat")
library("permute")

context("Testing shuffle()")

## test no permutation
test_that("shuffle(n) returns seq_len(n) when not permuting", {
    ctrl <- how(within = Within(type = "none"))

    expect_that(shuffle(3, control = ctrl), is_identical_to(seq_len(3)))
    expect_that(shuffle(1, control = ctrl), is_identical_to(1L))
})

## test shuffle returns integers
test_that("shuffle() returns integers", {
    ctrl <- how(within = Within(type = "none"))

    expect_that(shuffle(4), is_a("integer"))
    expect_that(shuffle(100), is_a("integer"))
    expect_that(shuffle(1, control = ctrl), is_identical_to(1L))
    expect_that(shuffle(3, control = ctrl), is_identical_to(c(1L, 2L, 3L)))
})

## test what shuffle returns when permuting only the strata
## must *not* assume that the samples are in contiguous blocks
test_that("shuffle() works for non-contigous blocks of samples", {
    ## permuting levels of Plots instead of observations
    ## non-contiguous blocks - checks that r1972 continues to work
    Plot <- factor(rep(1:4, 5))
    CTRL <- how(plots = Plots(strata = Plot, type = "free"),
                within = Within(type = "none"))
    n <- 20
    ## FIXME Temporary fix related to #25
    suppressWarnings(RNGversion("3.5.0"))
    set.seed(2)
    result <- shuffle(n, CTRL)
    out1 <- as.integer(c( 3, 2, 1, 4,
                          7, 6, 5, 8,
                         11,10, 9,12,
                         15,14,13,16,
                         19,18,17,20))
    expect_that(result, is_identical_to(out1))
    out2 <- factor(as.integer(rep(c(3,2,1,4), 5)), levels = 1:4)
    expect_that(Plot[result], is_identical_to(out2))
})

test_that("shuffle can permute both plots and within in presence of blocks", {
    ## Example from @LindsayVass on github #9
    control <- how(within = Within(type = "free"),
                   plots = Plots(strata = rep(gl(2,7),2), type = "free"),
                   blocks = gl(2, 14))
    permSet <- shuffle(28, control = control)
    expect_identical(length(permSet), 28L)
    expect_is(permSet, "integer")
})

test_that("constant within plots works", {
    fac <- gl(4, 5)
    ## series permutations
    ctrl <- how(within = Within(type = "series", constant = TRUE),
                plots = Plots(strata = fac, type = "none"))
    perm <- shuffle(length(fac), control = ctrl)
    expect_identical(length(perm), length(fac))
    expect_identical(length(perm), 20L)
    expect_is(perm, "integer")
    ## free/randomisation
    ctrl <- how(within = Within(type = "free", constant = TRUE),
                plots = Plots(strata = fac, type = "none"))
    perm <- shuffle(length(fac), control = ctrl)
    expect_identical(length(perm), length(fac))
    expect_identical(length(perm), 20L)
    expect_is(perm, "integer")
    ## spatial grid 3x3
    fac <- gl(4, 9)
    ctrl <- how(within = Within(type = "grid", nrow = 3, ncol = 3, constant = TRUE),
                plots = Plots(strata = fac, type = "none"))
    perm <- shuffle(length(fac), control = ctrl)
    expect_identical(length(perm), length(fac))
    expect_identical(length(perm), 36L)
    expect_is(perm, "integer")
})

test_that("shuffel works with objects passed to n", {
    obj <- 1:4
    p <- shuffle(obj)
    expect_is(p, "integer")
    expect_identical(length(p), 4L)
    obj <- as.integer(1:4)
    p <- shuffle(obj)
    expect_is(p, "integer")
    expect_identical(length(p), 4L)
    obj <- as.factor(1:4)
    p <- shuffle(obj)
    expect_is(p, "integer")
    expect_identical(length(p), 4L)
    obj <- letters[1:4]
    p <- shuffle(obj)
    expect_is(p, "integer")
    expect_identical(length(p), 4L)
    obj <- matrix(1:16, ncol = 4, nrow = 4)
    p <- shuffle(obj)
    expect_is(p, "integer")
    expect_identical(length(p), 4L)
    obj <- data.frame(A = 1:4, B = letters[1:4])
    p <- shuffle(obj)
    expect_is(p, "integer")
    expect_identical(length(p), 4L)
})
