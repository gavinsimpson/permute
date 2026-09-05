library("testthat")
library("permute")

context("Testing permutations()")

test_that("permutations applies the generated indices to data", {
    x <- c("oak", "ash", "elm", "yew")

    set.seed(42)
    indices <- shuffleSet(length(x), nset = 5, check = FALSE)
    expected <- matrix(x[indices], nrow = nrow(indices),
                       ncol = ncol(indices))

    set.seed(42)
    result <- permutations(x, nset = 5, check = FALSE)

    expect_identical(as.matrix(result), expected)
    expect_identical(class(result), c("permutations", "matrix"))
    expect_true(inherits(result, "matrix"))
    expect_identical(attr(result, "seed"), attr(indices, "seed"))
    expect_identical(attr(result, "control"), attr(indices, "control"))
})

test_that("permutations preserves seeded RNG compatibility", {
    suppressWarnings(RNGversion("3.5.0"))
    control <- how(
        plots = Plots(factor(c(1, 1, 1, 2, 2, 2)), type = "free"),
        within = Within(type = "free")
    )

    set.seed(42)
    actual <- as.matrix(permutations(letters[1:6], 3, control,
                                     check = FALSE))
    expected <- matrix(
        c("f", "d", "e", "c", "a", "b",
          "c", "a", "b", "f", "e", "d",
          "d", "e", "f", "b", "c", "a"),
        ncol = 6L, byrow = TRUE
    )
    expect_identical(actual, expected)
})

test_that("permutations follows restricted designs", {
    x <- letters[1:6]
    block <- gl(2, 3)
    control <- how(blocks = block)

    result <- permutations(x, nset = 10, control = control, check = FALSE)

    for (i in seq_len(nrow(result))) {
        expect_setequal(result[i, 1:3], x[1:3])
        expect_setequal(result[i, 4:6], x[4:6])
    }
})

test_that("permutations inherits shuffleSet row-count semantics", {
    x <- letters[1:4]
    control <- how(nperm = 3)
    result <- permutations(x, control = control, check = FALSE)
    expect_identical(nrow(result), 3L)

    result <- permutations(x[1:2], nset = 10, check = FALSE)
    expect_identical(nrow(result), 10L)

    control <- how(complete = TRUE)
    result <- permutations(x[1:3], nset = 20, control = control)
    expect_equal(nrow(result), factorial(3) - 1)
})

test_that("permutations handles single observations and permutations", {
    result <- permutations(42, nset = 3, check = FALSE)
    expect_identical(dim(result), c(3L, 1L))
    expect_identical(as.vector(as.matrix(result)), rep(42, 3))

    result <- permutations(42, nset = 3, quietly = TRUE)
    expect_identical(dim(result), c(0L, 1L))
    expect_output(print(result), "No. of Permutations: 0")

    result <- permutations(1:4, nset = 1, check = FALSE)
    expect_identical(dim(result), c(1L, 4L))
})

test_that("permutations supports unclassed atomic vectors and factors", {
    control <- how(within = Within(type = "none"))
    values <- list(
        logical = c(TRUE, FALSE, NA),
        integer = c(1L, 2L, NA_integer_),
        double = c(1, 2, NA_real_),
        complex = c(1+1i, 2+2i, NA_complex_),
        character = c("a", "b", NA_character_),
        raw = as.raw(1:3)
    )

    for (x in values) {
        result <- permutations(x, nset = 1, control = control, check = FALSE)
        expect_identical(as.vector(as.matrix(result)), x)
    }

    x <- factor(c("low", "high", NA), levels = c("low", "high"))
    result <- permutations(x, nset = 1, control = control, check = FALSE)
    expect_identical(as.vector(as.matrix(result)), as.character(x))
})

test_that("permutations rejects unsupported inputs", {
    expect_error(permutations(numeric(), 1), "non-empty")
    expect_error(permutations(list(1, 2), 1), "atomic vector")
    expect_error(permutations(matrix(1:4, 2), 1), "one-dimensional")
    expect_error(permutations(data.frame(x = 1:2), 1), "atomic vector")
    expect_error(permutations(as.Date("2020-01-01") + 0:1, 1), "unclassed")
})

test_that("permutations has print and coercion methods", {
    control <- how(blocks = gl(2, 2))
    result <- permutations(letters[1:4], nset = 2, control = control,
                           check = FALSE)

    expect_output(print(result), "No. of Permutations: 2")
    expect_output(print(result), "No. of Samples: 4")
    expect_output(print(result), "Restricted by Blocks:")
    expect_output(print(result), "p1")
    expect_output(print(result), "[a-d]")

    result_matrix <- as.matrix(result)
    expect_identical(class(result_matrix), c("matrix", "array"))
    expect_null(attr(result_matrix, "seed"))
    expect_null(attr(result_matrix, "control"))
    expect_identical(dim(result_matrix), dim(result))
})
