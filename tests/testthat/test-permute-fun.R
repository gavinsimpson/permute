library("testthat")
library("permute")

context("Testing permute() function")

test_that("permute() returns ith row of all perms", {
    h <- how()
    v <- 1:4
    ch <- check(v, h, quietly = TRUE)
    h <- ch$control
    p1 <- permute(1, n = length(v), control = h)
    expect_equal(length(p1), 4L)
    expect_is(p1, "integer")
    expect_equal(p1, c(1L, 2L, 4L, 3L))
    expect_equal(p1, getAllperms(h)[1, ])

    p21 <- permute(21, n = length(v), control = h)
    expect_equal(length(p21), 4L)
    expect_is(p21, "integer")
    expect_equal(p21, c(4L, 2L, 3L, 1L))
    expect_equal(p21, getAllperms(h)[21, ])
})

test_that("permute() returns a random permutation if no $allperms", {
    h <- how()
    v <- 1:10                           # want something big so no allperms
    p <- permute(10, n = length(v), control = h)
    expect_equal(length(p), length(v))
    expect_true(all(p >= 1))
    expect_true(all(p <= 10))

    setComplete(h) <- TRUE
    expect_warning(permute(10, n = length(v), control = h),
                   regexp = "Returning a random permutation")
})

test_that("permute() returns rows from supplied permutation matrices", {
    perms <- rbind(c(2L, 1L, 3L), c(3L, 2L, 1L))

    expect_identical(permute(1, perms = perms), perms[1, ])
    expect_identical(permute(2, n = 3, perms = perms), perms[2, ])
    expect_identical(permute(1, n = 1:3, perms = perms), perms[1, ])
    expect_identical(permute(1, n = data.frame(x = 1:3), perms = perms),
                     perms[1, ])

    pmat <- shuffleSet(5, nset = 3, check = FALSE)
    expect_identical(permute(2, perms = pmat), pmat[2, ])

    aperms <- allPerms(3)
    expect_identical(permute(1, perms = aperms), aperms[1, ])
})

test_that("permute() validates supplied permutations", {
    perms <- rbind(c(2L, 1L, 3L), c(3L, 2L, 1L))

    expect_error(permute(0, perms = perms), "select a row")
    expect_error(permute(3, perms = perms), "select a row")
    expect_error(permute(NA, perms = perms), "single, non-missing integer")
    expect_error(permute(1.5, perms = perms), "single, non-missing integer")
    expect_error(permute(1, n = 4, perms = perms), "does not match")
    expect_error(permute(1, perms = as.list(perms)), "numeric matrix")
    expect_error(permute(1, perms = matrix(character(), 0, 3)),
                 "numeric matrix")
    expect_error(permute(1, perms = matrix(numeric(), 1, 0)),
                 "at least one column")

    invalid <- list(
        c(1, 1, 3),
        c(0, 2, 3),
        c(1, 2, 4),
        c(1, 2, NA),
        c(1, 2, Inf),
        c(1, 2, 2.5)
    )
    for (x in invalid) {
        expect_error(permute(1, perms = matrix(x, nrow = 1)),
                     "every index")
    }

    values <- permutations(11:13, nset = 1, check = FALSE)
    expect_error(permute(1, perms = values), "permutation indices")
})

test_that("using supplied permutations does not advance the RNG", {
    perms <- rbind(c(2L, 1L, 3L), c(3L, 2L, 1L))
    set.seed(42)
    seed <- .Random.seed
    permute(1, perms = perms)
    expect_identical(.Random.seed, seed)
})

test_that("permutator() iterates over supplied permutations", {
    perms <- rbind(c(2L, 1L, 3L), c(3L, 2L, 1L))
    iter <- permutator(perms = perms)

    expect_true(is.function(iter))
    expect_identical(attr(iter, "nperm"), 2L)
    expect_identical(attr(iter, "n"), 3L)
    expect_identical(iter(), perms[1, ])
    expect_identical(iter(), perms[2, ])
    expect_null(iter())
    expect_null(iter())
})

test_that("permutator() instances have independent state", {
    perms <- rbind(c(2L, 1L, 3L), c(3L, 2L, 1L))
    first <- permutator(perms = perms)
    second <- permutator(perms = perms)

    expect_identical(first(), perms[1, ])
    expect_identical(first(), perms[2, ])
    expect_identical(second(), perms[1, ])
})

test_that("permutator() accepts empty permutation sets", {
    perms <- matrix(integer(), nrow = 0L, ncol = 1L)
    iter <- permutator(perms = perms)

    expect_identical(attr(iter, "nperm"), 0L)
    expect_identical(attr(iter, "n"), 1L)
    expect_null(iter())
})

test_that("permutator() generates permutations eagerly", {
    set.seed(42)
    seed <- .Random.seed
    iter <- permutator(5, nset = 3, check = FALSE)
    generated_seed <- .Random.seed

    expect_false(identical(generated_seed, seed))
    expect_identical(attr(iter, "nperm"), 3L)
    expect_identical(attr(iter, "n"), 5L)
    iter()
    expect_identical(.Random.seed, generated_seed)
})

test_that("permutator() follows shuffleSet generation semantics", {
    control <- how(nperm = 4, blocks = gl(2, 3))
    iter <- permutator(6, control = control, check = FALSE)

    expect_identical(attr(iter, "nperm"), 4L)
    for (i in seq_len(attr(iter, "nperm"))) {
        perm <- iter()
        expect_setequal(perm[1:3], 1:3)
        expect_setequal(perm[4:6], 4:6)
    }
    expect_null(iter())

    complete <- permutator(3, nset = 20, control = how(complete = TRUE))
    expect_equal(attr(complete, "nperm"), factorial(3) - 1)
})

test_that("permutator() validates its source at construction", {
    perms <- rbind(c(2L, 1L, 3L), c(3L, 2L, 1L))

    expect_error(permutator(perms = rbind(perms, c(1L, 1L, 3L))),
                 "every index")
    expect_error(permutator(), "'n' must be supplied")
    expect_error(permutator(3, perms = perms), "either 'perms'")
    expect_error(permutator(perms = perms, control = how()),
                 "either 'perms'")
})
