library("testthat")
library("permute")

context("Testing how()")

test_that("how() works with explicit NULL blocks arg", {
    ## Example of failure from Jari github #8
    h <- how(blocks = NULL)
    expect_that(h, is_a("how"))
})

test_that("print method for how", {
    expect_output(print(how()), regexp = "Permutation Design:")

    ctrl <- how(plots = Plots(strata = gl(4,5)))
    expect_output(print(how()), regexp = "Plots:")

    ctrl <- how(plots = Plots(strata = gl(4,9), type = "grid", ncol = 3, nrow = 3))
    expect_output(print(ctrl), regexp = "Grid dimensions:")

    ctrl <- how(blocks = gl(2, 2))
    expect_output(print(ctrl), regexp = "Blocks: gl")

    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))
    expect_output(print(ctrl), regexp = "Permutation type: partition")
    expect_output(print(ctrl), regexp = "Group sizes: 3, 2")
    expect_output(print(ctrl), regexp = "Within-group order retained")
})

test_that("Within preserves positional NULL arguments", {
    within <- Within("free", FALSE, FALSE, NULL, NULL, FALSE)
    call <- getCall(within)

    expect_true(all(c("ncol", "nrow") %in% names(call)))
    expect_null(call[["ncol"]])
    expect_null(call[["nrow"]])

    updated <- update(within, type = "series")
    expect_identical(getType(updated), "series")
    expect_null(getRow(updated))
    expect_null(getCol(updated))
})

test_that("update methods reject objects without constructor calls", {
    ctrl <- how()
    ctrl$call <- NULL
    expect_error(update(ctrl, nperm = 5), "need an object with call component")

    plots <- Plots(gl(2, 2))
    plots$call <- NULL
    expect_error(update(plots, type = "free"),
                 "need an object with call component")
})
