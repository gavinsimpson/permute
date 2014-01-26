library(testthat)
library_if_available(permute)

context("Testing allPerms()")

## test that allPerms returns
test_that("allPerms - blocks - within block free", {
    ## example data from Joris Meys from
    ## http://stackoverflow.com/a/21313632/429846
    thedata <- data.frame(score = c(replicate(4, sample(1:3))),
                          judge = rep(1:4, each = 3),
                          wine = rep.int(1:3, 4))

    ## without the observed permutation included
    hh <- how(within = Within("free"),
              blocks = factor(thedata$judge),
              complete = TRUE, maxperm = 1e9)
    nr <- nrow(thedata)
    np <- numPerms(nr, hh)
    p <- allPerms(nr, control = hh)
    expect_that(nrow(p), equals(np - 1)) ## default is to drop observed

    ## with the observed permutation included
    hh <- how(within = Within("free"),
              blocks = factor(thedata$judge),
              complete = TRUE, maxperm = 1e9,
              observed = TRUE)
    p <- allPerms(nr, control = hh)
    expect_that(nrow(p), equals(np)) ## now includes observed
})

test_that("allPerms - blocks - within block free - uneven block sizes", {
    fac <- factor(rep(1:3, times = c(2,2,4)))

    ## without the observed permutation included
    hh <- how(within = Within("free"),
              blocks = fac,
              complete = TRUE, maxperm = 1e9)
    ll <- length(fac)
    np <- numPerms(ll, hh)
    expect_that(np, equals(prod(factorial(2), factorial(2), factorial(4))))
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np - 1)) ## default is to drop observed

    ## with the observed permutation included
    hh <- how(within = Within("free"),
              blocks = fac,
              complete = TRUE, maxperm = 1e9,
              observed = TRUE)
    p <- allPerms(ll, control = hh)
    expect_that(nrow(p), equals(np)) ## now includes observed
})
