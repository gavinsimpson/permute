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

test_that("check rejects strata that do not match the observations", {
    expect_error(
        check(5, how(blocks = gl(2, 2))),
        "length of Block 'strata' do not match"
    )
    expect_error(
        check(5, how(plots = Plots(gl(2, 2)))),
        "length of Plot 'strata' do not match"
    )
})

test_that("check rejects invalid within-plot restrictions", {
    unbalanced <- factor(c(rep("a", 4), rep("b", 2)))
    ctrl <- how(
        plots = Plots(unbalanced),
        within = Within(type = "grid", nrow = 2, ncol = 2)
    )
    expect_error(check(6, ctrl), "Unbalanced 'grid' designs")

    ctrl <- how(
        plots = Plots(gl(2, 3)),
        within = Within(type = "grid", nrow = 2, ncol = 2)
    )
    expect_error(check(6, ctrl), "not a multiple of number of observations")

    ctrl <- how(
        plots = Plots(factor(c("a", "a", "b", "b", "b"))),
        within = Within(type = "series", constant = TRUE)
    )
    expect_error(check(5, ctrl), "constant = TRUE")
})

test_that("check requires permuted plots to be balanced", {
    plots <- factor(c("a", "a", "b", "b", "b"))
    ctrl <- how(
        plots = Plots(plots, type = "free"),
        within = Within(type = "none")
    )
    expect_error(check(5, ctrl), "balanced if permuting 'strata'")

    blocks <- factor(c("x", "x", "x", "y", "y", "y"))
    plots <- factor(c("a", "a", "b", "a", "b", "b"))
    ctrl <- how(
        blocks = blocks,
        plots = Plots(plots, type = "free"),
        within = Within(type = "none")
    )
    expect_error(check(6, ctrl), "balanced within blocks")
})

test_that("check validates supplied complete permutation sets", {
    ctrl <- how()
    ctrl$all.perms <- matrix(1:4, nrow = 1)

    expect_error(check(4, ctrl), "must be of class 'allPerms'")
})

test_that("check reports and records complete enumeration below minperm", {
    ctrl <- how(nperm = 2, minperm = 30, make = FALSE)

    expect_message(
        checked <- check(4, ctrl),
        "Set of permutations < 'minperm'"
    )
    expect_true(getComplete(checked$control))
    expect_equal(getMaxperm(checked$control), factorial(4))
    expect_equal(getNperm(checked$control), 2)
    expect_null(getAllperms(checked$control))
})

test_that("check and its summary have stable print contracts", {
    checked <- check(4, how(nperm = 2, minperm = 0, make = FALSE),
                     quietly = TRUE)

    expect_output(print(checked), "24")
    checked_summary <- summary(checked)
    expect_s3_class(checked_summary, "summary.check")
    expect_output(print(checked_summary),
                  "Number of possible permutations: 24")
    expect_output(print(checked_summary), "Permutation Design:")
    printed <- NULL
    capture.output(printed <- withVisible(print(checked_summary)))
    expect_false(printed$visible)
})
