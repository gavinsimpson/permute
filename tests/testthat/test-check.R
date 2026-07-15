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

## Example slightly modified from issue 27 by @dbaranger
test_that("balanced designs only required within blocks", {
    df27 <- data.frame(
        blocklevel = c(rep(1, 10), rep(2, 30)),
        plotlevel  = c(rep(c(1, 2), 5), rep(3:5, 10))
    )
    df27 <- df27[order(df27$plotlevel, df27$blocklevel), ]

    h <- with(df27,
        how(
            within = Within(type = "series"),
            blocks = blocklevel, 
            plots = Plots(strata = plotlevel, type = "series")
        )
    )

    expect_no_error(chk <- check(df27, control = h))
    expect_identical(chk$n, 150000)
})