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
