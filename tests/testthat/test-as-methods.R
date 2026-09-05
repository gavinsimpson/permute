library("testthat")
library("permute")

context("Testing as.foo() methods")

test_that("as.matrix allPerms method", {
    ap <- allPerms(1:3)
    m <- as.matrix(ap)
    expect_is(m, "matrix")
    expect_false(inherits(m, "allPerms"))
})

test_that("as.matrix permutationMatrix method", {
    perms <- shuffleSet(10, nset = 10)
    m <- as.matrix(perms)
    expect_is(m, "matrix")
    expect_false(inherits(m, "permutationMatrix"))
})

test_that("as.allPerms fixes #16", {
    res1 <- check(4, control = how())
    ctrl <- getControl(res1)
    res <- check(4, ctrl)
    expect_is(res, "check")
})

test_that("as.allPerms supports objects without a control", {
    perms <- as.allPerms(matrix(c(2L, 1L), nrow = 1))

    expect_s3_class(perms, "allPerms")
    expect_identical(attr(perms, "control"), NA)
    expect_identical(attr(perms, "observed"), NA)
})
