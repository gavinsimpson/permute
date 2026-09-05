library("testthat")
library("permute")

context("Testing formula group specifications")

group_data <- data.frame(
    block = factor(rep(c("b1", "b2"), each = 6)),
    site = factor(rep(rep(c("s1", "s2"), each = 3), 2)),
    plot = factor(rep(c("p1", "p2", "p3"), 4)),
    unused = factor(rep(c("u1", "u2"), 6),
                    levels = c("u1", "u2", "u3")),
    value = seq_len(12)
)

test_that("single grouping formulas produce factors", {
    ctrl <- how(plots = Plots(strata = ~ site), blocks = ~ block,
                data = group_data)
    standalone <- Plots(strata = ~ site, data = group_data)

    expect_identical(getBlocks(ctrl), droplevels(group_data$block))
    expect_identical(getStrata(ctrl), droplevels(group_data$site))
    expect_identical(getStrata(standalone), droplevels(group_data$site))
    expect_identical(ctrl$blocks.name, "block")
    expect_identical(getPlots(ctrl)$plots.name, "site")
    expect_false("data" %in% names(ctrl))
    expect_false("data" %in% names(standalone))
})

test_that("grouping getters expose formula-derived factors", {
    ctrl <- how(plots = Plots(~ site / plot), blocks = ~ block,
                data = group_data)
    expected_plots <- interaction(group_data$site, group_data$plot,
                                  drop = TRUE)
    expected_blocks <- droplevels(group_data$block)

    expect_identical(getPlots(ctrl), ctrl$plots)
    expect_identical(getStrata(ctrl), expected_plots)
    expect_identical(getStrata(ctrl, which = "plots", drop = FALSE),
                     expected_plots)
    expect_identical(getBlocks(ctrl), expected_blocks)
    expect_identical(getStrata(ctrl, which = "blocks"), expected_blocks)
    expect_identical(getStrata(ctrl, which = "blocks", drop = FALSE),
                     expected_blocks)
})

test_that("multiple and nested formula terms produce interactions", {
    expected <- interaction(group_data$site, group_data$plot, drop = TRUE)
    formulas <- list(
        ~ site + plot,
        ~ site / plot,
        ~ plot %in% site,
        ~ site:plot,
        ~ site * plot,
        ~ (site + plot)
    )

    for (f in formulas) {
        observed <- getStrata(Plots(f, data = group_data))
        observed_blocks <- getBlocks(how(blocks = f, data = group_data))
        expect_identical(outer(observed, observed, `==`),
                         outer(expected, expected, `==`))
        expect_identical(outer(observed_blocks, observed_blocks, `==`),
                         outer(expected, expected, `==`))
    }
})

test_that("dot expansion and term subtraction follow formula semantics", {
    dat <- group_data[c("site", "plot", "unused")]
    expected <- interaction(dat$site, dat$plot, drop = TRUE)
    plots <- Plots(~ . - unused, data = dat)

    expect_identical(getStrata(plots), expected)
})

test_that("formula expressions use model frame evaluation", {
    outside <- factor(rep(c("o1", "o2"), 6))
    site <- factor(rep("wrong", nrow(group_data)))
    expected <- interaction(group_data$site, outside, drop = TRUE)
    plots <- Plots(~ site + outside, data = group_data)
    transformed <- Plots(~ site + cut(value, breaks = 3), data = group_data)
    transformed_expected <- interaction(
        group_data$site,
        cut(group_data$value, breaks = 3),
        drop = TRUE
    )

    expect_identical(getStrata(plots), expected)
    expect_identical(getStrata(transformed), transformed_expected)
})

test_that("character and numeric grouping variables are converted to factors", {
    dat <- data.frame(
        character_group = rep(c("a", "b"), each = 3),
        numeric_group = rep(seq_len(3), 2)
    )
    expected <- interaction(factor(dat$character_group),
                            factor(dat$numeric_group), drop = TRUE)

    plots <- Plots(~ character_group + numeric_group, data = dat)
    ctrl <- how(blocks = ~ character_group + numeric_group, data = dat)

    expect_identical(getStrata(plots), expected)
    expect_identical(getBlocks(ctrl), expected)
})

test_that("formula evaluation retains rows and drops unused levels", {
    dat <- group_data
    dat$site[2] <- NA
    plots <- Plots(~ site + unused, data = dat)
    strata <- getStrata(plots)

    expect_length(strata, nrow(dat))
    expect_true(is.na(strata[2]))
    expect_false(any(grepl("u3", levels(strata), fixed = TRUE)))
})

test_that("nested Plots calls receive data only from how", {
    ctrl <- how(plots = Plots(~ site), data = group_data)
    qualified <- how(plots = permute::Plots(~ site), data = group_data)
    prebuilt <- Plots(~ site, data = group_data)

    expect_identical(getStrata(ctrl), droplevels(group_data$site))
    expect_identical(getStrata(qualified), droplevels(group_data$site))
    expect_identical(getStrata(how(plots = prebuilt)),
                     droplevels(group_data$site))
    expect_error(
        how(plots = Plots(~ site, data = group_data), data = group_data),
        "supply 'data' to 'how\\(\\)'"
    )

    other_data <- group_data
    other_data$site <- factor(rep(c("x", "y", "z"), each = 4))
    with_prebuilt <- how(plots = prebuilt, blocks = ~ block,
                         data = other_data)
    expect_identical(getStrata(update(with_prebuilt, nperm = 5)),
                     getStrata(prebuilt))
})

test_that("Plots preserves positional NULL arguments before data", {
    positional <- Plots(~ site, "none", FALSE, NULL, NULL, FALSE, group_data)
    named <- Plots(strata = ~ site, data = group_data)
    call <- getCall(positional)

    expect_identical(getStrata(positional), getStrata(named))
    expect_true(all(c("ncol", "nrow", "data") %in% names(call)))
    expect_null(call[["ncol"]])
    expect_null(call[["nrow"]])

    updated <- update(positional, type = "free")
    expect_identical(getStrata(updated), getStrata(named))
    expect_identical(getType(updated), "free")
})

test_that("invalid formula group specifications fail clearly", {
    expect_error(Plots(~ site), "'data' must be a data frame")
    expect_error(how(blocks = ~ block), "'data' must be a data frame")
    expect_error(Plots(~ site, data = as.list(group_data)),
                 "'data' must be a data frame")
    expect_error(Plots(site ~ plot, data = group_data),
                 "one-sided formula")
    expect_error(Plots(~ 1, data = group_data), "contain grouping terms")
    expect_error(Plots(~ missing_group, data = group_data),
                 "missing_group")
    expect_error(Plots(~ cbind(site, plot), data = group_data),
                 "one value per observation")
})

test_that("invalid block formulas fail clearly", {
    expect_error(how(blocks = ~ block), "'data' must be a data frame")
    expect_error(how(blocks = ~ block, data = as.list(group_data)),
                 "'data' must be a data frame")
    expect_error(how(blocks = block ~ site, data = group_data),
                 "one-sided formula")
    expect_error(how(blocks = ~ 1, data = group_data),
                 "contain grouping terms")
    expect_error(how(blocks = ~ missing_group, data = group_data),
                 "missing_group")
    expect_error(how(blocks = ~ cbind(site, plot), data = group_data),
                 "one value per observation")
})

test_that("formula designs remain updateable", {
    ctrl <- how(plots = Plots(~ site / plot), blocks = ~ block,
                data = group_data)
    settings <- update(ctrl, nperm = 17)
    changed_data <- group_data
    changed_data$block <- factor(rep(c("x", "y", "z"), each = 4))
    changed_data$site <- factor(rep(c("left", "right"), 6))
    changed <- update(ctrl, data = changed_data)
    changed_blocks <- update(ctrl, blocks = ~ site)

    expect_identical(getNperm(settings), 17)
    expect_identical(getBlocks(settings), getBlocks(ctrl))
    expect_identical(getStrata(settings), getStrata(ctrl))
    expect_identical(getBlocks(changed), changed_data$block)
    expect_identical(
        getStrata(changed),
        interaction(changed_data$site, changed_data$plot, drop = TRUE)
    )
    expect_identical(getBlocks(changed_blocks), group_data$site)
    expect_identical(changed_blocks$blocks.name, "site")

    plots <- Plots(~ site / plot, data = group_data)
    updated_plots <- update(plots, data = changed_data, type = "free")
    expect_identical(
        getStrata(updated_plots),
        interaction(changed_data$site, changed_data$plot, drop = TRUE)
    )
    expect_identical(getType(updated_plots), "free")
    expect_identical(updated_plots$plots.name, "site/plot")

    replaced_strata <- update(plots, strata = ~ block, data = group_data)
    expect_identical(getStrata(replaced_strata), group_data$block)
    expect_identical(replaced_strata$plots.name, "block")

    replaced_plots <- update(ctrl, plots = Plots(~ block), data = group_data)
    expect_identical(getStrata(replaced_plots), group_data$block)
    expect_identical(getPlots(replaced_plots)$plots.name, "block")
})

test_that("replacement setters interoperate with formula designs", {
    replacement <- factor(rep(c("g1", "g2"), 6),
                          levels = c("g1", "g2", "unused"))
    ctrl <- how(plots = Plots(~ site), blocks = ~ block,
                data = group_data)

    setBlocks(ctrl) <- replacement
    expect_identical(getBlocks(ctrl), replacement)
    expect_identical(getStrata(ctrl, which = "blocks", drop = FALSE),
                     replacement)
    expect_identical(getStrata(ctrl, which = "blocks"),
                     droplevels(replacement))
    expect_identical(getBlocks(update(ctrl, nperm = 17)), replacement)

    plots <- Plots(~ site, data = group_data)
    setStrata(plots) <- replacement
    expect_identical(getStrata(plots, drop = FALSE), replacement)
    expect_identical(getStrata(plots), droplevels(replacement))
    expect_identical(getStrata(update(plots, type = "free"), drop = FALSE),
                     replacement)

    setStrata(ctrl) <- replacement
    expect_identical(getStrata(ctrl, drop = FALSE), replacement)
    expect_identical(getStrata(update(ctrl, nperm = 23), drop = FALSE),
                     replacement)

    setBlocks(ctrl) <- NULL
    expect_null(getBlocks(ctrl))
    expect_null(getBlocks(update(ctrl, nperm = 29)))
})

test_that("setPlots keeps formula-built plots updateable", {
    expected <- interaction(group_data$site, group_data$plot, drop = TRUE)
    ctrl <- how(blocks = ~ block, data = group_data)
    plots <- Plots(~ site / plot, data = group_data)

    setPlots(ctrl) <- plots
    call <- getCall(ctrl)

    expect_identical(getPlots(ctrl), plots)
    expect_identical(getStrata(ctrl), expected)
    expect_false("data" %in% names(call[["plots"]]))
    expect_identical(call[["plots"]][["strata"]],
                     getStrata(plots, drop = FALSE))

    updated <- update(ctrl, nperm = 17)
    expect_identical(getNperm(updated), 17)
    expect_identical(getStrata(updated), expected)
    expect_identical(getBlocks(updated), group_data$block)

    changed_data <- group_data
    changed_data$block <- factor(rep(c("x", "y", "z"), each = 4))
    changed <- update(ctrl, data = changed_data)
    expect_identical(getStrata(changed), expected)
    expect_identical(getBlocks(changed), changed_data$block)
})

test_that("replacement setters reject unresolved formulas", {
    ctrl <- how()
    plots <- Plots()

    expect_error(setBlocks(ctrl) <- ~ block,
                 "how\\(blocks = \\.\\.\\., data = \\.\\.\\.\\)")
    expect_error(setStrata(ctrl) <- ~ site,
                 "Plots\\(strata = \\.\\.\\., data = \\.\\.\\.\\)")
    expect_error(setStrata(plots) <- ~ site,
                 "Plots\\(strata = \\.\\.\\., data = \\.\\.\\.\\)")
})

test_that("Plots updates support grouping representation transitions", {
    expected <- interaction(group_data$site, group_data$plot, drop = TRUE)
    vector_plots <- Plots(group_data$block)
    complex_formulas <- list(
        ~ site + plot,
        ~ site / plot,
        ~ plot %in% site,
        ~ . - block - unused - value
    )

    for (f in complex_formulas) {
        updated <- update(vector_plots, strata = f, data = group_data)
        observed <- getStrata(updated)
        expect_identical(outer(observed, observed, `==`),
                         outer(expected, expected, `==`))
    }

    formula_plots <- Plots(~ site / plot, data = group_data)
    as_vector <- update(formula_plots, strata = group_data$block)
    without_strata <- update(formula_plots, strata = NULL)

    expect_identical(getStrata(as_vector), group_data$block)
    expect_identical(as_vector$plots.name, "group_data$block")
    expect_null(getStrata(without_strata))

    chained <- update(
        update(vector_plots, strata = ~ site / plot, data = group_data),
        type = "free"
    )
    expect_identical(getStrata(chained), expected)
    expect_identical(getType(chained), "free")
})

test_that("how updates support grouping representation transitions", {
    expected <- interaction(group_data$site, group_data$plot, drop = TRUE)
    vector_ctrl <- how(plots = Plots(group_data$block),
                       blocks = group_data$block)
    complex_formulas <- list(
        ~ site + plot,
        ~ site / plot,
        ~ plot %in% site,
        ~ . - block - unused - value
    )

    for (f in complex_formulas) {
        updated_blocks <- update(vector_ctrl, blocks = f, data = group_data)
        updated_plots <- update(vector_ctrl, plots = Plots(f),
                                data = group_data)
        observed_blocks <- getBlocks(updated_blocks)
        observed_plots <- getStrata(updated_plots)
        expect_identical(outer(observed_blocks, observed_blocks, `==`),
                         outer(expected, expected, `==`))
        expect_identical(outer(observed_plots, observed_plots, `==`),
                         outer(expected, expected, `==`))
    }

    formula_ctrl <- how(plots = Plots(~ site / plot), blocks = ~ block,
                        data = group_data)
    vector_blocks <- update(formula_ctrl, blocks = group_data$site)
    no_blocks <- update(formula_ctrl, blocks = NULL)
    vector_plots <- update(formula_ctrl, plots = Plots(group_data$block))

    expect_identical(getBlocks(vector_blocks), group_data$site)
    expect_null(getBlocks(no_blocks))
    expect_identical(getStrata(vector_plots), group_data$block)

    changed_data <- group_data
    changed_data$block <- factor(rep(c("one", "two", "three"), each = 4))
    changed_data$site <- factor(rep(c("left", "right"), 6))
    simultaneous <- update(
        formula_ctrl,
        plots = Plots(~ site / plot),
        blocks = ~ block / site,
        data = changed_data
    )
    chained <- update(simultaneous, nperm = 23)

    expect_identical(
        getStrata(simultaneous),
        interaction(changed_data$site, changed_data$plot, drop = TRUE)
    )
    expect_identical(
        getBlocks(simultaneous),
        interaction(changed_data$block, changed_data$site, drop = TRUE)
    )
    expect_identical(getStrata(chained), getStrata(simultaneous))
    expect_identical(getBlocks(chained), getBlocks(simultaneous))
    expect_identical(getNperm(chained), 23)
})

test_that("formula updates propagate evaluation errors", {
    plots <- Plots(~ site / plot, data = group_data)
    ctrl <- how(plots = Plots(~ site / plot), blocks = ~ block,
                data = group_data)

    expect_error(update(plots, data = group_data["block"]), "site")
    expect_error(update(plots, data = as.list(group_data)),
                 "'data' must be a data frame")
    expect_error(update(plots, strata = site ~ plot, data = group_data),
                 "one-sided formula")
    expect_error(update(plots, strata = ~ 1, data = group_data),
                 "contain grouping terms")

    expect_error(update(ctrl, data = group_data["block"]), "site")
    expect_error(update(ctrl, data = NULL), "'data' must be a data frame")
    expect_error(update(ctrl, blocks = block ~ site), "one-sided formula")
    expect_error(update(ctrl, blocks = ~ 1), "contain grouping terms")
    expect_error(
        update(ctrl, plots = Plots(~ site, data = group_data)),
        "supply 'data' to 'how\\(\\)'"
    )
})

test_that("unevaluated formula updates retain reusable calls", {
    plots <- Plots(~ site / plot, data = group_data)
    ctrl <- how(plots = Plots(~ site / plot), blocks = ~ block,
                data = group_data)
    plots_call <- update(plots, type = "free", evaluate = FALSE)
    how_call <- update(ctrl, nperm = 17, evaluate = FALSE)

    expect_true(is.call(plots_call))
    expect_identical(plots_call[["type"]], "free")
    expect_true(all(c("strata", "data") %in% names(plots_call)))

    expect_true(is.call(how_call))
    expect_identical(how_call[["nperm"]], 17)
    expect_true(all(c("plots", "blocks", "data") %in% names(how_call)))
    expect_false("data" %in% names(how_call[["plots"]]))

    evaluated_plots <- eval(plots_call)
    evaluated_how <- eval(how_call)
    expect_identical(getType(evaluated_plots), "free")
    expect_identical(getNperm(evaluated_how), 17)
    expect_identical(getStrata(evaluated_how), getStrata(ctrl))
    expect_identical(getBlocks(evaluated_how), getBlocks(ctrl))
})

test_that("how materializes block formulas supplied through variables", {
    expected <- interaction(group_data$site, group_data$plot, drop = TRUE)

    constructed <- local({
        block_formula <- ~ site / plot
        how(blocks = block_formula, data = group_data)
    })
    constructed_call <- getCall(constructed)
    expect_true(inherits(constructed_call[["blocks"]], "formula"))

    reconstructed <- update(constructed, nperm = 11)
    expect_identical(getBlocks(reconstructed), expected)
    expect_identical(getNperm(reconstructed), 11)

    ctrl <- how(blocks = ~ block, data = group_data)
    replaced <- local({
        block_formula <- ~ site / plot
        update(ctrl, blocks = block_formula)
    })
    replaced_call <- getCall(replaced)
    expect_true(inherits(replaced_call[["blocks"]], "formula"))

    chained <- update(replaced, nperm = 23)
    expect_identical(getBlocks(chained), expected)
    expect_identical(getNperm(chained), 23)
    expect_identical(chained$blocks.name, "site/plot")
})

test_that("formula and factor designs generate the same permutations", {
    formula_ctrl <- how(plots = Plots(~ site), blocks = ~ block,
                        data = group_data)
    factor_ctrl <- how(plots = Plots(group_data$site),
                       blocks = group_data$block)

    set.seed(42)
    formula_perms <- shuffleSet(nrow(group_data), 10, formula_ctrl,
                                check = FALSE)
    set.seed(42)
    factor_perms <- shuffleSet(nrow(group_data), 10, factor_ctrl,
                               check = FALSE)

    formula_perms <- matrix(as.integer(formula_perms),
                            nrow = nrow(formula_perms))
    factor_perms <- matrix(as.integer(factor_perms),
                           nrow = nrow(factor_perms))
    expect_identical(formula_perms, factor_perms)
})
