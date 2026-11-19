library("testthat")
library("permute")

assignment_fixture <- function(sizes = c(2L, 5L, 3L, 4L)) {
    data.frame(
        plot_id = factor(rep(c("z", "a", "m", "b"), sizes)),
        treatment = factor(rep(c("T", "C", "T", "C"), sizes)),
        block = rep(c("one", "one", "two", "two"), sizes)
    )
}

assignment_control <- function(dat, ...) {
    how(plots = Plots(dat$plot_id, type = "free", variables = dat$treatment), ...)
}

## Independent oracle built from all subsets of treated plots.
expected_binary_assignments <- function(dat) {
    plots <- unique(dat$plot_id)
    first <- match(plots, dat$plot_id)
    treated <- sum(dat$treatment[first] == "T")
    choices <- combn(seq_along(plots), treated)
    apply(choices, 2L, function(choice) {
        paste(ifelse(dat$plot_id %in% plots[choice], "T", "C"), collapse = "")
    })
}

assignment_keys <- function(values) apply(values, 1L, paste, collapse = "")

test_that("free plot assignments enumerate the same six allocations at either balance", {
    for (sizes in list(c(2L, 5L, 3L, 4L), rep(3L, 4L))) {
        dat <- assignment_fixture(sizes)
        ctrl <- assignment_control(dat, observed = TRUE)
        expect_identical(getType(ctrl, which = "plots"), "free")
        expect_identical(getType(ctrl, which = "within"), "none")
        expect_equal(numPerms(nrow(dat), ctrl), 6)
        draws <- allPerms(nrow(dat), ctrl)
        expect_s3_class(draws, "plotPermutations")
        expect_false(is.matrix(draws))
        expect_identical(dim(draws), c(6L, nrow(dat)))
        expect_setequal(assignment_keys(permutedVariables(draws)),
                        expected_binary_assignments(dat))
        expect_equal(length(unique(assignment_keys(permutedVariables(draws)))), 6)
        expect_identical(permutedVariables(draws),
                         matrix(as.character(dat$treatment)[permutationIndices(draws)],
                                nrow = 6L, ncol = nrow(dat)))
    }
    dat <- assignment_fixture()
    values <- permutedVariables(allPerms(nrow(dat), assignment_control(dat, observed = TRUE)))
    expect_identical(sort(rowSums(values == "T")), c(5, 6, 7, 7, 8, 9))
    expect_error(check(nrow(dat), how(plots = Plots(dat$plot_id, type = "free"))),
                 "balanced")
})

test_that("observed exclusion compares plot assignments before expansion", {
    dat <- assignment_fixture()
    ctrl <- assignment_control(dat)
    draws <- allPerms(nrow(dat), ctrl)
    keys <- assignment_keys(permutedVariables(draws))
    observed <- paste(dat$treatment, collapse = "")
    expect_length(keys, 5L)
    expect_false(observed %in% keys)
    expect_setequal(keys, setdiff(expected_binary_assignments(dat), observed))

    checked <- getControl(check(nrow(dat), ctrl, quietly = TRUE))
    expect_equal(getNperm(checked), 5)
    expect_s3_class(getAllperms(checked), "plotPermutations")
    cached <- shuffleSet(nrow(dat), control = checked, quietly = TRUE)
    expect_setequal(assignment_keys(permutedVariables(cached)), keys)
    expect_identical(permutedVariables(permute(1, nrow(dat), checked)),
                     permutedVariables(draws)[1L, ])
    expect_error(permute(1, nrow(dat) + 1L, checked), "per data row")
})

test_that("blocking operates on plots and handles interleaved rows", {
    dat <- assignment_fixture()
    order <- c(14L, 3L, 1L, 8L, 4L, 11L, 2L, 5L, 9L, 12L, 6L, 13L, 7L, 10L)
    dat <- dat[order, ]
    ctrl <- assignment_control(dat, blocks = dat$block, observed = TRUE)
    expect_equal(numPerms(nrow(dat), ctrl), 4)
    values <- permutedVariables(allPerms(nrow(dat), ctrl))
    first <- which(!duplicated(dat$plot_id))
    for (block in unique(dat$block)) {
        take <- first[dat$block[first] == block]
        expect_true(all(rowSums(values[, take] == "T") == 1L))
    }
    for (plot in unique(dat$plot_id)) {
        take <- which(dat$plot_id == plot)
        expect_true(all(apply(values[, take, drop = FALSE], 1L,
                              function(x) length(unique(x)) == 1L)))
    }
    checked <- getControl(check(nrow(dat), ctrl, quietly = TRUE))
    expect_equal(nrow(shuffleSet(nrow(dat), control = checked, quietly = TRUE)), 4L)
})

test_that("formulas and updates retain resolved assignments", {
    dat <- assignment_fixture()
    ctrl <- how(plots = Plots(~ plot_id, type = "free", variables = ~ treatment),
                data = dat, observed = TRUE)
    expect_equal(numPerms(nrow(dat), ctrl), 6)
    expect_identical(permutedVariables(allPerms(nrow(dat), update(ctrl, nperm = 3))),
                     permutedVariables(allPerms(nrow(dat), ctrl)))
    standalone <- Plots(~ plot_id, type = "free", variables = ~ treatment, data = dat)
    ctrl2 <- how(plots = standalone, observed = TRUE)
    expect_identical(permutedVariables(allPerms(nrow(dat), update(ctrl2, nperm = 3))),
                     permutedVariables(allPerms(nrow(dat), ctrl)))
    changed <- update(standalone, variables = rep("C", nrow(dat)))
    expect_equal(numPerms(nrow(dat), how(plots = changed)), 1)
    expect_output(print(ctrl), "Variables: treatment")
})

test_that("all generators, values and iterators use the assignment contract", {
    dat <- assignment_fixture()
    ctrl <- assignment_control(dat, nperm = 3, minperm = 0)
    set.seed(9)
    first <- shuffle(nrow(dat), ctrl)
    set.seed(9)
    expect_identical(first, shuffle(nrow(dat), ctrl))
    expect_length(permutationIndices(first), nrow(dat))
    set.seed(8)
    draws <- shuffleSet(nrow(dat), 30, ctrl, check = FALSE)
    set.seed(8)
    expect_identical(draws, shuffleSet(nrow(dat), 30, ctrl, check = FALSE))
    expect_identical(dim(draws), c(30L, nrow(dat)))
    expect_true(all(assignment_keys(permutedVariables(draws)) %in%
                    expected_binary_assignments(dat)))
    expect_true(!is.null(attr(draws, "seed")))
    values <- permutations(dat$treatment, nset = 30, control = ctrl, check = FALSE)
    expect_identical(dim(values), c(30L, nrow(dat)))
    expect_error(permutations(seq_len(nrow(dat)), control = ctrl), "must match")
    iterator <- permutator(perms = draws)
    for (i in seq_len(nrow(draws))) {
        expect_identical(permutedVariables(iterator()), permutedVariables(draws)[i, ])
    }
    expect_null(iterator())
    generated <- permutator(nrow(dat), control = ctrl, check = FALSE)
    expect_s3_class(generated(), "plotPermutations")
    expect_s3_class(permute(1, perms = draws), "plotPermutations")
    expect_error(permute(31, perms = draws), "select a row")
    expect_error(as.matrix(draws), "constraint|model")
    expect_output(print(draws), "Plot variables expanded to 14")
})

test_that("validation cannot be bypassed by check FALSE", {
    dat <- assignment_fixture()
    invalid <- list(
        list(strata = dat$plot_id, variables = rep(NA_character_, nrow(dat))),
        list(strata = dat$plot_id, variables = "T"),
        list(strata = dat$plot_id, variables = seq_len(nrow(dat))),
        list(strata = replace(dat$plot_id, 1, NA), variables = dat$treatment),
        list(variables = dat$treatment),
        list(strata = dat$plot_id, variables = dat$treatment, type = "series")
    )
    for (args in invalid) {
        if (is.null(args$type)) args$type <- "free"
        ctrl <- how(plots = do.call(Plots, args))
        expect_error(shuffle(nrow(dat), ctrl))
        expect_error(shuffleSet(nrow(dat), 1, ctrl, check = FALSE))
        expect_error(numPerms(nrow(dat), ctrl, check = FALSE))
        expect_error(allPerms(nrow(dat), ctrl, check = FALSE))
    }
    ctrl <- how(plots = Plots(dat$plot_id, type = "free", variables = dat$treatment),
                within = Within(type = "free"))
    expect_error(shuffle(nrow(dat), ctrl), "requires Within")
    ctrl <- assignment_control(dat, blocks = rep(c("x", "y"), length.out = nrow(dat)))
    expect_error(numPerms(nrow(dat), ctrl), "exactly one block")
    expect_error(shuffle(nrow(dat) + 1L, assignment_control(dat)), "per data row")
    ctrl <- assignment_control(dat, maxperm = 2)
    expect_error(allPerms(nrow(dat), ctrl), "too large")
})

test_that("singleton plots and a single assignment label have well-defined counts", {
    dat <- data.frame(plot_id = c("a", "b", "c"), treatment = c("T", "C", "C"))
    ctrl <- assignment_control(dat, observed = TRUE)
    expect_equal(numPerms(3, ctrl), 3)
    expect_equal(nrow(allPerms(3, ctrl)), 3L)
    dat$treatment <- rep("C", 3L)
    ctrl <- assignment_control(dat)
    expect_equal(numPerms(3, ctrl), 1)
    expect_identical(dim(permutedVariables(allPerms(3, ctrl))), c(0L, 3L))
    expect_identical(permutedVariables(shuffle(3, ctrl)), dat$treatment)
    expect_equal(nrow(allPerms(3, update(ctrl, observed = TRUE))), 1L)
})

test_that("three treatment groups use the existing multinomial count", {
    dat <- data.frame(plot_id = rep(letters[1:4], c(1, 2, 3, 4)),
                      treatment = rep(c("A", "A", "B", "C"), c(1, 2, 3, 4)))
    ctrl <- assignment_control(dat, observed = TRUE)
    expect_equal(numPerms(nrow(dat), ctrl), 12)
    values <- permutedVariables(allPerms(nrow(dat), ctrl))
    expect_equal(length(unique(assignment_keys(values))), 12L)
})

test_that("plot-level allocations agree with exhaustive refits", {
    skip_if_not_installed("vegan")
    set.seed(1)
    sizes <- c(1L, 3L, 2L, 4L, 2L)
    dat <- data.frame(
        plot_id = rep(letters[1:5], sizes),
        treatment = rep(c("C", "T", "C", "T", "C"), sizes)
    )
    mat <- matrix(rexp(nrow(dat) * 5), nrow = nrow(dat))
    D <- vegan::vegdist(mat, method = "bray")
    ctrl <- assignment_control(dat, observed = TRUE)
    values <- permutedVariables(allPerms(nrow(dat), ctrl))
    statistic <- function(labels) {
        data <- data.frame(exposure = factor(labels))
        vegan::adonis2(D ~ exposure, data = data, permutations = 0)$R2[1L]
    }
    proposed <- apply(values, 1, statistic)
    ## Enumerate every choice of two plots independently of permute.
    choices <- combn(unique(dat$plot_id), 2L)
    reference <- apply(choices, 2, function(treated) {
        statistic(ifelse(dat$plot_id %in% treated, "T", "C"))
    })
    expect_equal(nrow(values), choose(5, 2))
    expect_equal(sort(proposed), sort(reference), tolerance = 1e-12)
    observed <- statistic(dat$treatment)
    expect_equal(mean(proposed >= observed - 1e-12),
                 mean(reference >= observed - 1e-12))

    ## Two groups exchanged in their entirety produce tied statistics.
    two <- data.frame(subject = rep(c("a", "b"), c(2, 5)),
                      exposure = rep(c(0, 1), c(2, 5)))
    ctrl <- how(plots = Plots(two$subject, type = "free", variables = two$exposure),
                observed = TRUE)
    two_values <- permutedVariables(allPerms(7, ctrl))
    D2 <- dist(seq_len(7))
    r2 <- apply(two_values, 1, function(exposure) {
        vegan::adonis2(D2 ~ exposure, data = data.frame(exposure),
                      permutations = 0)$R2[1L]
    })
    expect_equal(r2[1], r2[2])
    expect_equal(mean(r2 >= r2[1] - 1e-12), 1)

    expect_error(suppressMessages(vegan::adonis2(D ~ treatment, data = dat,
                                                permutations = assignment_control(dat))),
                 "constraint|model")
})

test_that("deduplicated assignments have the distribution of free plot shuffles", {
    dat <- assignment_fixture()
    first <- which(!duplicated(dat$plot_id))
    labels <- as.character(dat$treatment[first])
    full <- allPerms(length(first), how(observed = TRUE))
    full_keys <- apply(full, 1L, function(i) paste(labels[i], collapse = ""))
    ctrl <- assignment_control(dat, observed = TRUE)
    values <- permutedVariables(allPerms(nrow(dat), ctrl))
    unique_keys <- assignment_keys(values[, first, drop = FALSE])
    expect_equal(nrow(full), 24L)
    expect_setequal(names(table(full_keys)), unique_keys)
    expect_true(all(table(full_keys) == 4L))

    ## Permuting the data rows or renaming plots preserves the assignment set.
    order <- c(14L, 3L, 1L, 8L, 4L, 11L, 2L, 5L, 9L, 12L, 6L, 13L, 7L, 10L)
    reordered <- dat[order, ]
    renamed <- reordered
    renamed$plot_id <- factor(paste0("plot", match(renamed$plot_id, unique(renamed$plot_id))))
    reordered_values <- permutedVariables(allPerms(nrow(dat), assignment_control(reordered, observed = TRUE)))
    renamed_values <- permutedVariables(allPerms(nrow(dat), assignment_control(renamed, observed = TRUE)))
    expect_setequal(assignment_keys(reordered_values[, order(order)]), assignment_keys(values))
    expect_setequal(assignment_keys(renamed_values), assignment_keys(reordered_values))
})

test_that("assignment caches round trip and reject mismatched designs", {
    dat <- assignment_fixture()
    ctrl <- assignment_control(dat)
    setAllperms(ctrl) <- allPerms(nrow(dat), ctrl)
    setComplete(ctrl) <- TRUE
    expect_equal(nrow(shuffleSet(nrow(dat), control = ctrl, quietly = TRUE)), 5L)
    expect_equal(length(permutedVariables(permute(1, nrow(dat), ctrl))), nrow(dat))
    ctrl$plots$variables <- ctrl$plots$variables[nrow(dat):1, , drop = FALSE]
    expect_error(shuffle(nrow(dat), ctrl), "constant within|cached assignments")

    single <- factor(rep("C", 3))
    expect_identical(dim(allPartitions(single)), c(0L, 3L))
    expect_identical(dim(allPartitions(single, how(observed = TRUE))), c(1L, 3L))
})

test_that("multiple plot variables are partitioned as complete typed rows", {
    sizes <- c(2L, 5L, 3L, 4L, 1L, 2L)
    dat <- data.frame(plot = rep(letters[1:6], sizes),
                      A = factor(rep(c('a', 'a', 'a', 'b', 'b', 'b'), sizes)),
                      B = ordered(rep(c('x', 'x', 'y', 'x', 'y', 'y'), sizes)),
                      x = rep(c(1, 1, 2, 3, 4, 4), sizes))
    h <- how(plots = Plots(~ plot, type = 'free', variables = ~ A + B + x),
             data = dat, observed = TRUE)
    expect_equal(numPerms(nrow(dat), h), factorial(6)/(factorial(2)^2))
    draws <- allPerms(nrow(dat), h)
    expect_equal(nrow(draws), 180L)
    values <- permutedVariables(draws)
    expect_length(values, 180L)
    expect_identical(names(values[[1L]]), c('A','B','x'))
    expect_true(is.factor(values[[1L]]$A))
    expect_true(is.ordered(values[[1L]]$B))
    expect_type(values[[1L]]$x, 'double')
    expect_identical(permutedVariables(draws, 1), values[[1L]])
    first <- which(!duplicated(dat$plot))
    original <- dat[first, c('A','B','x')]
    keys <- vapply(values, function(value) paste(as.matrix(value[first, ]),collapse='|'),character(1))
    expect_equal(length(unique(keys)), 180L)
    expect_true(all(vapply(values, function(value) {
        identical(sort(value$x[first]),sort(original$x))
    },logical(1))))
    h2 <- how(plots = Plots(dat$plot, type='free', variables=dat[c('A','B','x')]),observed=TRUE)
    expect_identical(permutationIndices(allPerms(nrow(dat),h)),
                     permutationIndices(allPerms(nrow(dat),h2)))
})

test_that("distinct numeric profiles retain precision beyond printed labels", {
    id <- rep(letters[1:3],each=2)
    x <- rep(c(1,1+1e-15,2),each=2)
    h <- how(plots=Plots(id,type='free',variables=data.frame(x=x)),observed=TRUE)
    expect_equal(numPerms(length(id),h),6)
    values <- permutedVariables(allPerms(length(id),h))
    expect_true(all(apply(values,1,function(x) identical(sort(x),sort(rep(c(1,1+1e-15,2),each=2))))))
})

test_that("Monte Carlo draws are distinct from complete enumeration metadata", {
    dat <- assignment_fixture()
    ctrl <- assignment_control(dat, observed = TRUE, complete = TRUE)
    sampled <- shuffleSet(nrow(dat), 6, ctrl, check = FALSE)
    expect_false(isTRUE(attr(sampled, "observed")))
    enumerated <- suppressMessages(shuffleSet(nrow(dat), control = ctrl))
    expect_true(isTRUE(attr(enumerated, "observed")))
})
