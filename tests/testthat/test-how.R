library("testthat")
library("permute")

context("Testing how()")

test_that("how() works with explicit NULL blocks arg", {
    ## Example of failure from Jari github #8
    h <- how(blocks = NULL)
    expect_that(h, is_a("how"))
})

test_that("namespace-qualified how calls can be updated and shuffled", {
    ## Reproduction of GitHub issue #43
    ctrl <- permute::how(blocks = gl(5, 4))
    expect_identical(getCall(ctrl)[[1L]], quote(permute::how))
    expect_equal(getNperm(update(ctrl, nperm = 9)), 9)

    set.seed(43)
    expected <- shuffle(20, control = how(blocks = gl(5, 4)))
    set.seed(43)
    actual <- permute::shuffle(20, control = ctrl)

    expect_identical(actual, expected)
    expect_identical(sort(actual), seq_len(20))
    expect_identical(getBlocks(ctrl)[actual], getBlocks(ctrl))
})

test_that("namespace-qualified Within calls can be updated", {
    within <- permute::Within(type = "series", ncol = NULL, nrow = NULL)
    expect_identical(getCall(within)[[1L]], quote(permute::Within))

    updated <- update(within, mirror = TRUE)
    expect_identical(getType(updated), "series")
    expect_true(getMirror(updated))
    expect_true(all(c("ncol", "nrow") %in% names(getCall(updated))))
    expect_null(getRow(updated))
    expect_null(getCol(updated))
})

test_that("namespace-qualified Plots calls can be updated", {
    plots <- permute::Plots(strata = gl(5, 4), type = "free")
    expect_identical(getCall(plots)[[1L]], quote(permute::Plots))

    updated <- update(plots, type = "none")
    expect_identical(getType(updated), "none")
    expect_identical(getStrata(updated), getStrata(plots))
    expect_identical(updated$plots.name, plots$plots.name)
})

test_that("nested namespace-qualified constructors can be updated and shuffled", {
    controls <- list(
        how(within = permute::Within(type = "series"),
            plots = permute::Plots(strata = gl(5, 4)), blocks = gl(5, 4)),
        permute::how(within = permute::Within(type = "series"),
                     plots = permute::Plots(strata = gl(5, 4)),
                     blocks = gl(5, 4))
    )

    set.seed(43)
    expected <- shuffle(20, control = how(within = Within(type = "series"),
                                        plots = Plots(strata = gl(5, 4)),
                                        blocks = gl(5, 4)))
    for (ctrl in controls) {
        updated <- update(ctrl, nperm = 9)
        expect_equal(getNperm(updated), 9)
        expect_identical(getCall(getWithin(updated))[[1L]],
                         quote(permute::Within))
        expect_identical(getCall(getPlots(updated))[[1L]],
                         quote(permute::Plots))

        set.seed(43)
        expect_identical(permute::shuffle(20, control = updated), expected)
    }
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
