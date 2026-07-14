library("testthat")
library("permute")

context("Testing check()")

## test that check will return all perms including the observed
test_that("check returns observed ordering in set of all permutations *if* asked to", {
    ## simple permutation
    h <- how(observed = TRUE)
    pp <- check(4, control = h)
    expect_that(nrow(pp$control$all.perms), equals(factorial(4)))

    ## time series
    h <- how(within = Within(type = "series"), observed = TRUE)
    n <- 10
    pp <- check(n, control = h)
    expect_that(nrow(pp$control$all.perms), equals(n))

    ## time series specified as a vector
    h <- how(within = Within(type = "series"), observed = TRUE)
    n <- 10
    vec <- seq_len(n)
    pp <- check(vec, control = h)
    expect_that(nrow(pp$control$all.perms), equals(n))
})

## test that check throws an error when within and plot permutation
## type is "none"
test_that("check detects if nothing to permute", {
    pl <- gl(4, 3)
    n <- 12
    h <- how(within = Within(type = "none"),
             plots = Plots(strata = pl, type = "none"))
    expect_error(check(seq_len(n), control = h),
                 regexp = "Permutation 'type' is \"none\" for both 'plots' & 'within'.\nNothing to permute.")
})

## test that check accepts a valid plot-level grid design and rejects
## one whose grid dimensions do not match the number of Plots (see the
## bug where 'levels(Plots)' returned NULL for every design)
test_that("check accepts a valid plot-level grid design", {
    ## 9 Plots arranged as a 3 x 3 grid; 25 observations per Plot
    pl <- gl(9, 25)
    n <- length(pl)
    h <- how(within = Within(type = "free"),
             plots = Plots(strata = pl, type = "grid", nrow = 3, ncol = 3))
    expect_that(check(seq_len(n), control = h), is_a("check"))
})

test_that("check rejects a plot-level grid design with mismatched dimensions", {
    ## only 4 Plots, but the grid is 3 x 3 == 9 cells
    pl <- gl(4, 9)
    n <- length(pl)
    h <- how(within = Within(type = "free"),
             plots = Plots(strata = pl, type = "grid", nrow = 3, ncol = 3))
    expect_error(check(seq_len(n), control = h),
                 regexp = "Plot 'nrow' \\* 'ncol' not a multiple of number of Plots.")
})
