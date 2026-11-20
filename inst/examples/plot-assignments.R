library("permute")
library("vegan")

## One assignment and plot ID per original data row.
dat <- data.frame(
    plot_id = rep(LETTERS[1:4], c(2, 5, 3, 4)),
    treatment = rep(c("T", "C", "T", "C"), c(2, 5, 3, 4))
)
control <- how(
    plots = Plots(~ plot_id, type = "free", variables = ~ treatment),
    data = dat, complete = TRUE, observed = TRUE
)
assignments <- allPerms(nrow(dat), control)
print(assignments)
print(sort(rowSums(permutedVariables(assignments) == "T")))

## Expanded source row indices select from the original treatment column.
set.seed(4)
one_draw <- shuffle(nrow(dat), control)
stopifnot(identical(dat$treatment[permutationIndices(one_draw)],
                    permutedVariables(one_draw)))

## Fixed response distances, with a new assignment column for each fit.
D <- dist(c(1, 2, 4, 5, 8, 11, 12, 13, 16, 17, 18, 20, 25, 30))
statistic <- function(treatment) {
    metadata <- data.frame(treatment = factor(treatment))
    adonis2(D ~ treatment, data = metadata, permutations = 0)$R2[1L]
}
observed <- statistic(dat$treatment)
null <- apply(permutedVariables(assignments), 1L, statistic)
tolerance <- sqrt(.Machine$double.eps) * max(1, abs(observed))
print(data.frame(R2 = observed, exact_p = mean(null >= observed - tolerance)))

## Balanced plots use exactly the same interface and six plot assignments.
balanced <- data.frame(plot_id = rep(LETTERS[1:4], each = 3),
                        treatment = rep(c("T", "C", "T", "C"), each = 3))
balanced_control <- how(
    plots = Plots(~ plot_id, type = "free", variables = ~ treatment),
    data = balanced, observed = TRUE
)
stopifnot(numPerms(nrow(balanced), balanced_control) == 6)
stopifnot(all(rowSums(permutedVariables(allPerms(nrow(balanced), balanced_control)) == "T") == 6))

## Blocks constrain assignment of whole plots within the randomization blocks.
dat$block <- rep(c("one", "one", "two", "two"), c(2, 5, 3, 4))
blocked <- how(
    plots = Plots(~ plot_id, type = "free", variables = ~ treatment),
    blocks = ~ block, data = dat, observed = TRUE
)
stopifnot(numPerms(nrow(dat), blocked) == 4)
print(permutedVariables(allPerms(nrow(dat), blocked)))

## For larger designs, request independent Monte Carlo assignments.
set.seed(42)
random <- shuffleSet(nrow(dat), nset = 99, control = control, check = FALSE)
null_mc <- apply(permutedVariables(random), 1L, statistic)
print((1 + sum(null_mc >= observed - tolerance)) / (length(null_mc) + 1))
