library("testthat")
library("permute")

context("Testing symmetric grid permutations")

grid_permutations <- function(nr, nc, mirror = FALSE, symmetric = FALSE) {
    orientations <- list(c(FALSE, FALSE))
    if(mirror) {
        orientations <- c(orientations,
                          list(c(TRUE, FALSE), c(FALSE, TRUE)))
        if(!symmetric)
            orientations <- c(orientations, list(c(TRUE, TRUE)))
    }

    out <- vector("list", length(orientations) * nr * nc)
    idx <- 1L
    for(flip in orientations) {
        for(i in seq_len(nr)) {
            for(j in seq_len(nc)) {
                ir <- seq(i, length = nr) %% nr
                ic <- seq(j, length = nc) %% nc
                if(flip[1L])
                    ir <- rev(ir)
                if(flip[2L])
                    ic <- rev(ic)
                out[[idx]] <- rep(ic, each = nr) * nr +
                    rep(ir, length.out = nr * nc) + 1L
                idx <- idx + 1L
            }
        }
    }
    unique(do.call(rbind, out))
}

row_key <- function(x) {
    apply(x, 1L, paste, collapse = ",")
}

test_that("symmetric is stored, updated, and available through accessors", {
    w <- Within(type = "grid", nrow = 3, ncol = 3)
    p <- Plots(type = "grid", nrow = 3, ncol = 3)
    expect_false(getSymmetric(w))
    expect_false(getSymmetric(p))

    w <- update(w, symmetric = TRUE)
    p <- update(p, symmetric = TRUE)
    expect_true(getSymmetric(w))
    expect_true(getSymmetric(p))

    setSymmetric(w) <- FALSE
    setSymmetric(p) <- FALSE
    expect_false(getSymmetric(w))
    expect_false(getSymmetric(p))

    legacy_w <- Within(type = "grid", nrow = 3, ncol = 3)
    legacy_p <- Plots(type = "grid", nrow = 3, ncol = 3)
    legacy_w$symmetric <- NULL
    legacy_p$symmetric <- NULL
    expect_false(getSymmetric(legacy_w))
    expect_false(getSymmetric(legacy_p))

    ctrl <- how(within = Within(type = "grid", nrow = 3, ncol = 3,
                                symmetric = TRUE),
                plots = Plots(strata = gl(9, 1), type = "grid",
                              nrow = 3, ncol = 3, symmetric = TRUE))
    expect_true(getSymmetric(ctrl, which = "within"))
    expect_true(getSymmetric(ctrl, which = "plots"))
    expect_error(setSymmetric(ctrl) <- TRUE,
                 regexp = "can not be used directly")
})

test_that("how and permutation matrices print symmetric grid status", {
    ctrl <- how(within = Within(type = "grid", nrow = 3, ncol = 3,
                                mirror = TRUE, symmetric = TRUE),
                minperm = 0)
    txt <- capture.output(print(ctrl))
    expect_equal(sum(grepl("Symmetric\\?: Yes", txt)), 1L)

    plots <- Plots(strata = gl(9, 1), type = "grid", nrow = 3, ncol = 3,
                   mirror = TRUE, symmetric = TRUE)
    ctrl <- how(plots = plots, within = Within(type = "none"), minperm = 0)
    txt <- capture.output(print(ctrl))
    expect_equal(sum(grepl("Symmetric\\?: Yes", txt)), 1L)

    perms <- shuffleSet(9, nset = 2, control = how(
        within = Within(type = "grid", nrow = 3, ncol = 3,
                        mirror = TRUE, symmetric = TRUE), minperm = 0))
    expect_output(print(perms), regexp = "; symmetric")
})

test_that("shuffleGrid excludes simultaneous flips when symmetric", {
    args <- list(nrow = 3, ncol = 3, mirror = TRUE,
                 start.row = 1, start.col = 1)
    row_flip <- do.call(shuffleGrid, c(args, list(flip = c(TRUE, FALSE))))
    col_flip <- do.call(shuffleGrid, c(args, list(flip = c(FALSE, TRUE))))
    both <- do.call(shuffleGrid, c(args, list(flip = c(TRUE, TRUE))))

    set.seed(10)
    sym <- do.call(shuffleGrid, c(args, list(flip = c(TRUE, TRUE),
                                             symmetric = TRUE)))
    expect_true(identical(sym, row_flip) || identical(sym, col_flip))
    expect_false(identical(sym, both))

    plain <- shuffleGrid(3, 3, start.row = 1, start.col = 1)
    ignored <- shuffleGrid(3, 3, mirror = FALSE, start.row = 1,
                           start.col = 1, flip = c(TRUE, TRUE),
                           symmetric = TRUE)
    expect_identical(ignored, plain)

    allowed <- row_key(grid_permutations(3, 3, TRUE, TRUE))
    set.seed(11)
    draws <- replicate(100, shuffleGrid(3, 3, mirror = TRUE,
                                        symmetric = TRUE))
    expect_true(all(row_key(t(draws)) %in% allowed))
})

test_that("grid enumeration is complete, valid, and distinct", {
    cases <- list(
        c(3, 3, 9, 36, 27),
        c(2, 3, 6, 12, 12),
        c(4, 2, 8, 16, 16),
        c(2, 2, 4, 4, 4),
        c(1, 4, 4, 8, 8),
        c(4, 1, 4, 8, 8)
    )

    for(case in cases) {
        nr <- case[1L]
        nc <- case[2L]
        options <- list(c(FALSE, FALSE, case[3L]),
                        c(TRUE, FALSE, case[4L]),
                        c(TRUE, TRUE, case[5L]))
        for(option in options) {
            mirror <- as.logical(option[1L])
            symmetric <- as.logical(option[2L])
            expected_n <- option[3L]
            ctrl <- how(within = Within(type = "grid", nrow = nr,
                                        ncol = nc, mirror = mirror,
                                        symmetric = symmetric),
                        observed = TRUE)
            perms <- allPerms(nr * nc, control = ctrl)
            actual <- as.matrix(perms)
            expected <- grid_permutations(nr, nc, mirror, symmetric)

            expect_equal(numPerms(nr * nc, ctrl), expected_n)
            expect_equal(nrow(actual), expected_n)
            expect_equal(nrow(unique(actual)), expected_n)
            expect_setequal(row_key(actual), row_key(expected))
        }
    }
})

test_that("symmetric grids propagate through sampling designs", {
    expected <- row_key(grid_permutations(3, 3, TRUE, TRUE))
    within <- Within(type = "grid", nrow = 3, ncol = 3,
                     mirror = TRUE, symmetric = TRUE)
    ctrl <- how(within = within, minperm = 0)

    set.seed(12)
    expect_true(row_key(matrix(shuffle(9, ctrl), nrow = 1L)) %in% expected)
    set.seed(13)
    draws <- shuffleSet(9, nset = 20, control = ctrl)
    expect_true(all(row_key(draws) %in% expected))

    plots <- Plots(strata = gl(9, 1), type = "grid", nrow = 3, ncol = 3,
                   mirror = TRUE, symmetric = TRUE)
    plot_ctrl <- how(plots = plots, within = Within(type = "none"),
                     minperm = 0)
    set.seed(14)
    draws <- shuffleSet(9, nset = 20, control = plot_ctrl)
    expect_true(all(row_key(draws) %in% expected))

    constant_ctrl <- how(
        plots = Plots(strata = gl(2, 9)),
        within = Within(type = "grid", nrow = 3, ncol = 3,
                        mirror = TRUE, symmetric = TRUE, constant = TRUE),
        minperm = 0)
    set.seed(15)
    draws <- shuffleSet(18, nset = 20, control = constant_ctrl)
    expect_true(all(vapply(seq_len(nrow(draws)), function(i) {
        identical(draws[i, 1:9], draws[i, 10:18] - 9L)
    }, logical(1))))
})

test_that("allPerms preserves symmetric controls in nested grid designs", {
    plot_only <- how(
        plots = Plots(strata = gl(9, 1), type = "grid", nrow = 3, ncol = 3,
                      mirror = TRUE, symmetric = TRUE),
        within = Within(type = "none"), observed = TRUE)
    perms <- allPerms(9, plot_only)
    expect_equal(numPerms(9, plot_only), 27)
    expect_equal(nrow(perms), 27)
    expect_equal(nrow(unique(as.matrix(perms))), 27)

    plot_grid <- Plots(strata = gl(4, 4), type = "grid", nrow = 2, ncol = 2,
                       mirror = TRUE, symmetric = TRUE)
    constant <- Within(type = "grid", nrow = 2, ncol = 2, mirror = TRUE,
                       symmetric = TRUE, constant = TRUE)
    ctrl <- how(plots = plot_grid, within = constant, observed = TRUE)
    perms <- allPerms(16, ctrl)
    expect_equal(numPerms(16, ctrl), 16)
    expect_equal(nrow(perms), 16)
    expect_equal(nrow(unique(as.matrix(perms))), 16)

    independent <- update(constant, constant = FALSE)
    ctrl <- update(ctrl, within = independent)
    perms <- allPerms(16, ctrl)
    expect_equal(numPerms(16, ctrl), 1024)
    expect_equal(nrow(perms), 1024)
    expect_equal(nrow(unique(as.matrix(perms))), 1024)

    blocked <- how(
        plots = Plots(strata = factor(rep(seq_len(4), 2)), type = "grid",
                      nrow = 2, ncol = 2, mirror = TRUE, symmetric = TRUE),
        within = Within(type = "none"), blocks = gl(2, 4), observed = TRUE)
    perms <- allPerms(8, blocked)
    expect_equal(numPerms(8, blocked), 16)
    expect_equal(nrow(perms), 16)
    expect_equal(nrow(unique(as.matrix(perms))), 16)
})
