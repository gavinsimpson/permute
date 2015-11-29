library("testthat")
library("permute")

context("Testing set methods")

test_that("default methods for get functions", {
    v <- 1:10
    expect_error(setNperm(v) <- 10, regexp = "No default method")
    expect_error(setMaxperm(v) <- 10, regexp = "No default method")
    expect_error(setMinperm(v) <- 10, regexp = "No default method")
    expect_error(setComplete(v) <- TRUE, regexp = "No default method")
    expect_error(setBlocks(v) <- TRUE, regexp = "No default method")
    expect_error(setAllperms(v) <- 1:10, regexp = "No default method")
    expect_error(setObserved(v) <- TRUE, regexp = "No default method")
    expect_error(setPlots(v) <- Plots(), regexp = "No default method")
    expect_error(setWithin(v) <- Within(), regexp = "No default method")
    expect_error(setStrata(v) <- gl(2, 5), regexp = "No default method")
    expect_error(setRow(v) <- 4, regexp = "No default method")
    expect_error(setCol(v) <- 5, regexp = "No default method")
    expect_error(setDim(v) <- c(2,3), regexp = "No default method")
    expect_error(setType(v) <- "series", regexp = "No default method")
    expect_error(setMirror(v) <- TRUE, regexp = "No default method")
    expect_error(setMake(v) <- TRUE, regexp = "No default method")
    expect_error(setConstant(v) <- "series", regexp = "No default method")
})

test_that("how() set methods throw errors where not appropriate for use", {
    h <- how()
    expect_error(setDim(h) <- c(2,3), regexp = "can not be used directly on '\"how\"' objects")
    expect_error(setMirror(h) <- TRUE, regexp = "can not be used directly on '\"how\"' objects")
    expect_error(setConstant(h) <- TRUE, regexp = "can not be used directly on '\"how\"' objects")
    expect_error(setType(h) <- "series", regexp = "can not be used directly on '\"how\"' objects")
    expect_error(setRow(h) <- 2, regexp = "can not be used directly on '\"how\"' objects")
    expect_error(setCol(h) <- 3, regexp = "can not be used directly on '\"how\"' objects")
})

test_that("set within for class how works", {
    h <- how()
    setWithin(h) <- Within(type = "series")
    expect_identical(getType(h, which = "within"), "series")
})

test_that("set plots for class how works", {
    h <- how()
    setPlots(h) <- Plots(type = "series")
    expect_identical(getType(h, which = "plots"), "series")
})

test_that("test setMinperm work", {
    h <- how()
    setMinperm(h) <- 999
    expect_is(h, "how")
    expect_equal(getMinperm(h), 999)
})

test_that("test setMaxperm work", {
    h <- how()
    nperm <- 99999
    setMaxperm(h) <- nperm
    expect_is(h, "how")
    expect_equal(getMaxperm(h), nperm)
})

test_that("test setStrata works", {
    h <- how()
    f <- gl(5,5)
    ## `setStrata<-.how`(h, f)
    setStrata(h) <- f
    expect_is(h, "how")
    expect_identical(getStrata(h), f)
})
