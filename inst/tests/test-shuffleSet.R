library(testthat)
library_if_available(permute)

context("Testing shuffleSet()")

## test that shuffleSet interleves the separate block-level
## permutations correctly back into the original ordering
## This always generates odd, even, odd, ..., numbered vector
## of observations, hence when we take the modulus 2 we get
## a vector of 1,0,1,0,1,...
test_that("shuffleSet interleves block-level perms correctly", {
    gr <- factor(rep(1:2, length=20))
    ctrl <- how(nperm=5, blocks=gr)
    p <- shuffleSet(20, control = ctrl) %% 2
    y <- rep_len(c(1L, 0L), ncol(p))
    nc <- ncol(p)
    for (i in seq_len(nrow(p))) {
        expect_that(p[i, ], equals(y))
    }
})
