library("testthat")
library("permute")

context("Testing partition permutations")

is_canonical_partition <- function(perm, strata) {
    all(vapply(levels(strata), function(level) {
        ind <- which(strata == level)
        identical(perm[perm %in% ind], ind)
    }, logical(1)))
}

test_that("partition is a Plots type and defaults within to none", {
    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))

    expect_identical(getType(ctrl, which = "plots"), "partition")
    expect_identical(getType(ctrl, which = "within"), "none")

    plots <- getPlots(ctrl)
    setType(plots) <- "free"
    setType(plots) <- "partition"
    expect_identical(getType(plots), "partition")
})

test_that("explicit within permutations are rejected", {
    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(
        plots = Plots(groups, type = "partition"),
        within = Within(type = "free")
    )

    expect_error(check(length(groups), ctrl), "requires")
    expect_error(shuffle(length(groups), ctrl), "requires")
    expect_error(shuffleSet(length(groups), 2, ctrl, check = FALSE),
                 "requires")
    expect_error(allPerms(length(groups), ctrl, check = FALSE), "requires")
    expect_error(numPerms(length(groups), ctrl, check = FALSE), "requires")
})

test_that("partition designs require valid strata", {
    ctrl <- how(plots = Plots(type = "partition"))
    expect_error(check(5, ctrl), "strata")

    groups <- factor(c("a", "a", NA, "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))
    expect_error(check(length(groups), ctrl), "missing values")

    groups <- factor(c("a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))
    expect_error(shuffle(5, ctrl), "do not match")
})

test_that("numPerms counts distinct fixed-size assignments", {
    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))

    expect_equal(numPerms(length(groups), ctrl), 10)

    jackals <- gl(2, 10)
    ctrl <- how(plots = Plots(jackals, type = "partition"))
    expect_equal(numPerms(length(jackals), ctrl), choose(20, 10))
})

test_that("shuffle returns canonical index permutations", {
    groups <- factor(c("a", "b", "a", "a", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))

    set.seed(42)
    first <- shuffle(length(groups), ctrl)
    set.seed(42)
    second <- shuffle(length(groups), ctrl)

    expect_identical(first, second)
    expect_identical(sort(first), seq_along(groups))
    expect_equal(unname(table(groups[first])), unname(table(groups)))
    expect_true(is_canonical_partition(first, groups))
})

test_that("allPerms enumerates every assignment exactly once", {
    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))
    observed <- update(ctrl, observed = TRUE)

    all <- allPerms(length(groups), observed)
    without_observed <- allPerms(length(groups), ctrl)

    expect_is(all, "allPerms")
    expect_is(all, "permutationMatrix")
    expect_identical(dim(all), c(10L, 5L))
    expect_identical(dim(without_observed), c(9L, 5L))
    expect_identical(nrow(unique(as.matrix(all))), 10L)
    expect_true(all(apply(all, 1L, is_canonical_partition,
                          strata = groups)))

    assignments <- apply(all, 1L, function(i) paste(groups[i], collapse = ""))
    expect_identical(length(unique(assignments)), 10L)
    expect_identical(sum(apply(all, 1L, function(i) {
        identical(i, seq_along(groups))
    })), 1L)
})

test_that("shuffleSet uses existing complete-set behaviour", {
    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"))

    complete <- suppressMessages(shuffleSet(length(groups), control = ctrl))
    expect_identical(dim(complete), c(9L, 5L))
    expect_identical(nrow(unique(as.matrix(complete))), 9L)

    set.seed(7)
    random <- shuffleSet(length(groups), 20, ctrl, check = FALSE)
    expect_identical(dim(random), c(20L, 5L))
    expect_true(all(apply(random, 1L, is_canonical_partition,
                          strata = groups)))
})

test_that("partition permutations work independently within blocks", {
    groups <- factor(c("a", "b", "a", "b"))
    blocks <- factor(c("x", "x", "y", "y"))
    ctrl <- how(plots = Plots(groups, type = "partition"),
                blocks = blocks, observed = TRUE)

    expect_equal(numPerms(length(groups), ctrl), 4)

    all <- allPerms(length(groups), ctrl)
    expect_identical(dim(all), c(4L, 4L))
    expect_true(all(apply(all, 1L, function(i) {
        identical(table(groups[i][blocks == "x"]),
                  table(groups[blocks == "x"])) &&
            identical(table(groups[i][blocks == "y"]),
                      table(groups[blocks == "y"]))
    })))
})

test_that("partition convenience functions use the integrated API", {
    groups <- factor(c("a", "a", "a", "b", "b"))

    set.seed(2)
    first <- shufflePartition(groups)
    set.seed(2)
    expect_identical(first, shufflePartition(groups))

    all <- allPartitions(groups)
    set <- suppressMessages(shufflePartitionSet(groups, nset = 4))

    expect_is(all, "allPerms")
    expect_is(set, "permutationMatrix")
    expect_identical(dim(all), c(9L, 5L))
    expect_identical(dim(set), c(4L, 5L))
})

test_that("partition permutation matrices can pass through vegan", {
    skip_if_not_installed("vegan")
    groups <- factor(c("a", "a", "a", "b", "b"))
    ctrl <- how(plots = Plots(groups, type = "partition"), nperm = 4,
                minperm = 0)

    out <- vegan:::getPermuteMatrix(ctrl, length(groups))
    expect_is(out, "permutationMatrix")
    expect_identical(dim(out), c(4L, 5L))
})
