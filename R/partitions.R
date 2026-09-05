## Utilities for random assignments of observations to groups of fixed size.
## Each distinct arrangement of the group labels is represented by one
## canonical permutation of observation indices. The relative order of the
## observations originally in each group is retained.

`checkPartitionStrata` <- function(strata) {
    if (is.null(strata)) {
        stop("'strata' must be supplied for partition permutations")
    }
    if (anyNA(strata)) {
        stop("missing values are not allowed in partition 'strata'")
    }

    strata <- droplevels(as.factor(strata))
    if (nlevels(strata) < 1L) {
        stop("partition 'strata' must contain at least one group")
    }
    strata
    }

`checkPartitionDesign` <- function(control, n = NULL) {
    if (identical(getType(control, which = "plots"), "partition")) {
        if (!identical(getType(control, which = "within"), "none")) {
            stop("'Plots(type = \"partition\")' requires ",
                 "'Within(type = \"none\")'")
        }
        strata <- checkPartitionStrata(getStrata(control, which = "plots"))
        if (!is.null(n) && length(strata) != n) {
            stop("number of observations and length of partition 'strata' ",
                 "do not match")
        }
    }
    invisible(control)
}

## How many partitions for the grouping variable `strata`
`numPartitions` <- function(strata) {
    strata <- checkPartitionStrata(strata)
    tab <- table(strata)
    round(
      exp(lfactorial(length(strata)) - sum(lfactorial(tab))),
      0
    )
}

## Return the canonical index permutation corresponding to a target
## arrangement of the group labels.
`partitionPermutation` <- function(strata, target) {
    strata <- checkPartitionStrata(strata)
    target <- factor(target, levels = levels(strata))

    if (anyNA(target) || !isTRUE(all(table(target) == table(strata)))) {
        stop("'target' must be an arrangement of partition 'strata'")
    }

    source <- split(seq_along(strata), strata)
    used <- integer(nlevels(strata))
    target <- as.integer(target)
    out <- integer(length(strata))

    for (i in seq_along(target)) {
        group <- target[i]
        used[group] <- used[group] + 1L
        out[i] <- source[[group]][used[group]]
    }
    out
    }

`doShufflePartition` <- function(strata) {
    strata <- checkPartitionStrata(strata)
    target <- strata[sample.int(length(strata))]
    partitionPermutation(strata, target)
}

`doAllPartitions` <- function(strata, nperms = numPartitions(strata)) {
    strata <- checkPartitionStrata(strata)
    n <- length(strata)
    source <- split(seq_len(n), strata)
    remaining <- as.integer(table(strata))
    used <- integer(length(remaining))
    current <- integer(n)
    out <- matrix(NA_integer_, nrow = nperms, ncol = n)
    row <- 0L

    visit <- function(i) {
        if (i > n) {
            row <<- row + 1L
            out[row, ] <<- current
            return(invisible(NULL))
        }

        for (group in seq_along(remaining)) {
            if (remaining[group] > 0L) {
                remaining[group] <<- remaining[group] - 1L
                used[group] <<- used[group] + 1L
                current[i] <<- source[[group]][used[group]]
                visit(i + 1L)
                used[group] <<- used[group] - 1L
                remaining[group] <<- remaining[group] + 1L
            }
        }
        invisible(NULL)
    }

    visit(1L)
    out
}

`partitionControl` <- function(strata, control) {
    strata <- checkPartitionStrata(strata)
    setPlots(control) <- Plots(strata = strata, type = "partition")
    setWithin(control) <- Within(type = "none")
    setAllperms(control) <- NULL
    control
}

`allPartitions` <- function(strata, control = how(), check = TRUE) {
    control <- partitionControl(strata, control)
    allPerms(length(strata), control = control, check = check)
}

`shufflePartition` <- function(strata, control = how()) {
    control <- partitionControl(strata, control)
    shuffle(length(strata), control = control)
}

`shufflePartitionSet` <- function(strata, nset, control = how(),
                                  check = TRUE, quietly = FALSE) {
    control <- partitionControl(strata, control)
    if (missing(nset)) {
        shuffleSet(length(strata), control = control, check = check,
                   quietly = quietly)
    } else {
        shuffleSet(length(strata), nset = nset, control = control,
                   check = check, quietly = quietly)
    }
}
