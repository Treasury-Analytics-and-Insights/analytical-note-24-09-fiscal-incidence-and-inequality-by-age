## Path and file name definitions and functions used by the analysis scripts.


library(data.table)

# Define input and output path and file names
DATA_DIR    <- "../data"
WEIGHTS_DIR <- "" # Directory containing the TAWA replicate weights
WEIGHTS_FILE <- "" # Filename of the TAWA replicate weights
OUTPUT_DIR <- "../output"


# Attach the replicate weights to the records table
attach_replicate_weights <- function(records_tab, weights_tab) {
  weighted_tab <- weights_tab[records_tab, on = .(H_ID)]

  rep_names <- setdiff(names(weights_tab), "H_ID")

  replicated_tab <- melt(weighted_tab,
                         id.vars = setdiff(names(weighted_tab), rep_names),
                         variable.name = "replicate",
                         value.name = "weight")

  replicated_tab
}


# Construct a factor variable column indicating the age group to which each individual belongs
make_age_bins <- function(tab, age_col, bin_width, max_age, upper_lim = 200L) {
  tab[, age_bin := cut(get(age_col),
                       breaks = c(seq(0L, max_age, bin_width), upper_lim),
                       right = FALSE)]

  age_bin_lvls <- tab[, levels(age_bin)]

  age_bin_lvls[age_bin_lvls == paste0("[", max_age, ",", upper_lim, ")")] <-
    paste0("[", max_age, "+)")

  setattr(tab$age_bin, "levels", age_bin_lvls)

  rm(age_bin_lvls)

  setnames(tab, "age_bin", paste0("age_bin", bin_width))

  tab
}


# Calculate the absolute sampling error of a statistic from its replicated values
calc_ase <- function(col) {
  out <- 1.96 * sqrt(sum((col - first(col))^2, na.rm = TRUE) / (length(col) - 1L))

  out
}


# Set flags indicating large relative sampling errors
# Optionally flag relative sampling errors larger than the suppression threshold rather than
# indicating that they should be suppressed
set_rse_flags <- function(tab, col, flag_suffix = "flag", rse_thresh = 0.21, sup_thresh = 0.5,
                          dont_suppress = FALSE) {
  tab[, (paste(col, flag_suffix, sep = "_")) := ""]

  tab[abs(get(paste(col, "ase", sep = "_")) / get(col)) >= rse_thresh,
      (paste(col, flag_suffix, sep = "_")) := "*"]

  if (dont_suppress == TRUE) {
    tab[abs(get(paste(col, "ase", sep = "_")) / get(col)) > sup_thresh,
        (paste(col, flag_suffix, sep = "_")) := "+"]
  } else {
    tab[abs(get(paste(col, "ase", sep = "_")) / get(col)) > sup_thresh,
        (paste(col, flag_suffix, sep = "_")) := "S"]
  }

  tab
}


# Set a flag indicating that the population should be suppressed where either the sample count or
# population fail to meet specified thresholds
set_pop_flags <-
  function(tab, samp_col, pop_col, samp_thresh = 6L, pop_thresh = 3000, flag_suffix = "flag") {
    tab[get(samp_col) < samp_thresh | get(pop_col) < pop_thresh,
        (paste(pop_col, flag_suffix, sep = "_")) := "S"]

    tab
  }


# Coalesce two flag columns (indicating a large relative sampling error, or suppression) together,
# with the more severe flag taking precedence in each case
combine_flags <- function(tab, col, flag_suffix = "flag", flag2_suffix = "flag2") {
  # If either value is "S", use that
  tab[get(paste(col, flag2_suffix, sep = "_")) == "S",
      (paste(col, flag_suffix, sep = "_")) := "S"]

  # Otherwise, if either value is "+", use that
  tab[get(paste(col, flag_suffix, sep = "_")) != "S" &
        get(paste(col, flag2_suffix, sep = "_")) == "+",
      (paste(col, flag_suffix, sep = "_")) := "+"]

  # Otherwise, if either value is "*", use that
  tab[!(get(paste(col, flag_suffix, sep = "_")) %in% c("S", "+")) &
        (get(paste(col, flag_suffix, sep = "_")) == "*" |
           get(paste(col, flag2_suffix, sep = "_")) == "*"),
      (paste(col, flag_suffix, sep = "_")) := "*"]

  tab[, (paste(col, flag2_suffix, sep = "_")) := NULL]

  tab
}


# Calculate the total and mean values of monetary amounts for records grouped by a specified column
# (usually age bins) in each replicate
# Optionally equivalise amounts by a supplied equivalence scale column
totals_and_means_by_replicate <-
  function(tab, col, label, group_col = "age_bin5", equiv_col = FALSE) {
    if (equiv_col != FALSE) {
      tab_ <- tab[, .SD, .SDcols = c(group_col, "replicate", "weight", col, equiv_col)]

      setnames(tab_, equiv_col, "equiv")
    } else {
      tab_ <- tab[, .SD, .SDcols = c(group_col, "replicate", "weight", col)]

      tab_[, equiv := 1]
    }

    setnames(tab_, c(group_col, col), c("group_col", "col"))

    out <-
      tab_[, .(total = as.numeric(lapply(.SD, \(.) sum(weight * . / equiv))),
               mean = as.numeric(lapply(.SD, \(.) Hmisc::wtd.mean(. / equiv, weights = weight))),
               pop = sum(weight),
               .N),
           by = .(group_col, replicate),
           .SDcols = !c("weight", "equiv")]

    setorder(out, "replicate", "group_col")

    setnames(out, c("group_col", "total", "mean"),
             c(group_col, paste(c("total", "mean"), label, sep = "_")))

    out
  }


# Calculate the point estimates and absolute sampling errors of total and mean monetary amounts for
# records grouped by a specified column (usually age bins)
# Optionally include the corresponding estimates for the population in each group
totals_and_means_with_ase <-
  function(tab, col, label, round_tot = -6L, round_pop = -3L, include_pop = FALSE,
           group_col = "age_bin5", equiv_col = FALSE) {
    tab_ <-
      totals_and_means_by_replicate(tab, col, label, group_col = group_col, equiv_col = equiv_col)

    setnames(tab_, group_col, "group_col")

    means <- tab_[replicate %like% "Rep0", .SD,
                  .SDcols = c("group_col", "pop", paste(c("total", "mean"), label, sep = "_"), "N")]

    setnames(tab_, paste(c("total", "mean"), label, sep = "_"), c("total", "mean"))

    ase <- tab_[, .(pop_ase = calc_ase(pop),
                    total_ase = calc_ase(total),
                    mean_ase = calc_ase(mean),
                    mean_ase_rnd = calc_ase(round(total, round_tot) / round(pop, round_pop))),
                by = group_col]

    setnames(ase, "mean_ase_rnd", paste("mean", label, "ase_rnd", sep = "_"))
    setnames(ase, c("total_ase", "mean_ase"), paste(c("total", "mean"), label, "ase", sep = "_"))

    out <- means[ase, on = .(group_col)]

    setnames(out, "group_col", group_col)

    setcolorder(out, c(group_col, "N", "pop", "pop_ase",
                       paste("total", label, sep = "_"),
                       paste("total", label, "ase", sep = "_"),
                       paste("mean", label, sep = "_"),
                       paste("mean", label, "ase", sep = "_"),
                       paste("mean", label, "ase_rnd", sep = "_")))

    if (!include_pop) {
      out[, `:=`(pop = NULL, pop_ase = NULL)]
    }

    out
  }


# Calculate the Gini coefficients of monetary amounts for records grouped by a specified column
# (usually age bins) in each replicate
# Optionally equivalise amounts by a supplied equivalence scale column
ginis_by_replicate <- function(tab, col, label, group_col = "age_bin5", equiv_col = FALSE) {
  if (equiv_col != FALSE) {
    tab_ <- tab[, .SD, .SDcols = c(group_col, "replicate", "weight", col, equiv_col)]

    setnames(tab_, equiv_col, "equiv")
  } else {
    tab_ <- tab[, .SD, .SDcols = c(group_col, "replicate", "weight", col)]

    tab_[, equiv := 1]
  }

  setnames(tab_, c(group_col, col), c("group_col", "col"))

  out <-
    tab_[, .(gini = reldist::gini(pmax(col, 0) / equiv, weights = weight), .N),
         by = .(group_col, replicate)]

  setorder(out, "replicate", "group_col")

  setnames(out, c("gini", "group_col"), c(label, group_col))

  out
}


# Calculate the point estimates and absolute sampling errors of Gini coefficients of monetary
# amounts for records grouped by a specified column (usually age bins)
ginis_with_ase <-
  function(tab, col, label, scale_factor = 100, group_col = "age_bin5", equiv_col = FALSE) {
    tab_ <- ginis_by_replicate(tab, col, label, group_col = group_col, equiv_col = equiv_col)

    setnames(tab_, c(group_col, label), c("group_col", "gini"))

    pt_ests <- tab_[replicate %like% "Rep0$", .SD, .SDcols = c("group_col", "N", "gini")]

    ase <- tab_[, .(gini_ase = calc_ase(gini)), by = group_col]

    out <- pt_ests[ase, on = .(group_col)]

    out[is.na(gini), gini_ase := NA_real_]

    out[, (c("gini", "gini_ase")) := lapply(.SD, \(.) scale_factor * .),
        .SDcols = c("gini", "gini_ase")]

    setnames(out, c("group_col", "gini", "gini_ase"),
             c(group_col, label, paste(label, "ase", sep = "_")))

    out
  }


# Calculate the redistributive effect (difference in Gini coefficients) between two different
# monetary amounts for records grouped by a specified column (usually age bins) in each replicate
# Optionally equivalise amounts by a supplied equivalence scale column
redist_effects_by_replicate <-
  function(tab, col1, col2, label, group_col = "age_bin5", equiv_col = FALSE) {
    if (equiv_col != FALSE) {
      tab_ <- tab[, .SD, .SDcols = c(group_col, "replicate", "weight", col1, col2, equiv_col)]

      setnames(tab_, equiv_col, "equiv")
    } else {
      tab_ <- tab[, .SD, .SDcols = c(group_col, "replicate", "weight", col1, col2)]

      tab_[, equiv := 1]
    }

    setnames(tab_, c(group_col, col1, col2), c("group_col", "col1", "col2"))

    out <-
      tab_[, .(re = reldist::gini(pmax(col1, 0) / equiv, weights = weight) -
                 reldist::gini(pmax(col2, 0) / equiv, weights = weight),
               .N),
           by = .(group_col, replicate)]

    setorder(out, "replicate", "group_col")

    setnames(out, c("re", "group_col"), c(label, group_col))

    out
  }


# Calculate the point estimates and associated sampling errors of the redistributive effect
# (difference in Gini coefficients) between two different monetary amounts
redist_effects_with_ase <-
  function(tab, col1, col2, label, scale_factor = 100, group_col = "age_bin5", equiv_col = FALSE) {
    tab_ <- redist_effects_by_replicate(tab, col1, col2, label, group_col = group_col,
                                        equiv_col = equiv_col)

    setnames(tab_, c(group_col, label), c("group_col", "re"))

    pt_ests <- tab_[replicate %like% "Rep0$", .SD, .SDcols = c("group_col", "N", "re")]

    ase <- tab_[, .(re_ase = calc_ase(re)), by = group_col]

    out <- pt_ests[ase, on = .(group_col)]

    out[is.na(re), re_ase := NA_real_]

    out[, (c("re", "re_ase")) := lapply(.SD, \(.) scale_factor * .), .SDcols = c("re", "re_ase")]

    setnames(out, c("group_col", "re", "re_ase"),
             c(group_col, label, paste(label, "ase", sep = "_")))

    out
  }


# Round total and mean monetary amounts, and the populations on which they are based, applying
# suppression where required
round_totals_and_means <-
  function(tab, round_tot = -6L, round_pop = -3L, round_mean = -1L,
           samp_thresh = 6L, pop_thresh = 3000, dont_suppress = c()) {
    # Initialise the rounded table as a copy of the unrounded table
    tab_rounded <- copy(tab)

    # Round the population and the associated ASE
    tab_rounded[, `:=`(pop = round(pop, round_pop),
                       pop_ase = round(pop_ase, round_pop))]

    # Round the total amounts
    total_cols <- names(tab_rounded)[names(tab_rounded) %like% "^total_"]
    total_cols <- total_cols[!(total_cols %like% "_ase$")]
    if (length(total_cols) > 0L) {
      total_ase_cols <- paste(total_cols, "ase", sep = "_")
    } else {
      total_ase_cols <- total_cols
    }

    tab_rounded[, (total_cols) := lapply(.SD, \(.) round(., round_tot)), .SDcols = total_cols]
    tab_rounded[, (total_ase_cols) := lapply(.SD, \(.) round(., round_tot)),
                .SDcols = total_ase_cols]

    # Round the mean amounts
    mean_cols <- gsub("total_", "mean_", total_cols)
    tab_rounded[, (mean_cols) := lapply(.SD, \(.) round(. / pop, round_mean)), .SDcols = total_cols]

    # Substitute the mean amount ASEs calculated from rounded numerators and denominators in each
    # replicate and round them
    if (length(mean_cols) > 0L) {
      ase_cols <- paste(mean_cols, "ase", sep = "_")
      tab_rounded[, (ase_cols) := tab[, .SD, .SDcols = paste(ase_cols, "rnd", sep = "_")]]
      tab_rounded[, (ase_cols) := lapply(.SD, \(.) round(., round_mean)), .SDcols = ase_cols]
    }

    # Add flags for the population and total amounts based on their RSEs to the unrounded table
    for (col in c("pop", total_cols)) {
      if (col %in% dont_suppress) {
        tab <- set_rse_flags(tab, col, dont_suppress = TRUE)
      } else {
        tab <- set_rse_flags(tab, col)
      }
    }

    # Add flags indicating sample counts or weighted populations below the thresholds to the
    # unrounded table
    tab <- set_pop_flags(tab, "N", "pop", samp_thresh = samp_thresh, pop_thresh = pop_thresh)

    # Add flags for the mean amounts based on their RSEs to the unrounded table
    for (col in mean_cols) {
      if (col %in% dont_suppress) {
        tab <- set_rse_flags(tab, col, dont_suppress = TRUE)
      } else {
        tab <- set_rse_flags(tab, col)
      }
    }

    # Bind the flag columns of the unrounded table onto the rounded table
    flag_cols <- c("pop_flag")
    if (length(total_cols) > 0L) {
      flag_cols <- c(flag_cols, paste(total_cols, "flag", sep = "_"))
    }
    if (length(mean_cols) > 0L) {
      flag_cols <- c(flag_cols, paste(mean_cols, "flag", sep = "_"))
    }
    tab_rounded <- cbind(tab_rounded, tab[, .SD, .SDcols = flag_cols])

    # Add secondary flags for the mean amounts based on the RSEs obtained from the rounded means
    # and the rounded ASEs to the rounded table
    for (col in mean_cols) {
      if (col %in% dont_suppress) {
        tab_rounded <- set_rse_flags(tab_rounded, col, flag_suffix = "flag2", dont_suppress = TRUE)
      } else {
        tab_rounded <- set_rse_flags(tab_rounded, col, flag_suffix = "flag2")
      }
    }

    # Take the flag on each mean amount to be the more severe of the flags obtained from the rounded
    # and unrounded values
    for (col in mean_cols) {
      tab_rounded <- combine_flags(tab_rounded, col)
    }

    # Suppress population, total and mean amounts where indicated by their flags
    for (col in c("pop", total_cols, mean_cols)) {
      ase_col <- paste(col, "ase", sep = "_")
      tab_rounded[get(paste(col, "flag", sep = "_")) == "S", (c(col, ase_col)) := NA_real_]
    }

    # Where the population is suppressed, suppress the total and mean amounts based on it, and the
    # associated sample count
    for (col in c(total_cols, mean_cols)) {
      ase_col <- paste(col, "ase", sep = "_")
      tab_rounded[pop_flag == "S", (c(col, ase_col)) := NA_real_]
      tab_rounded[pop_flag == "S", (paste(col, "flag", sep = "_")) := "S"]
    }

    tab_rounded[pop_flag == "S", N := NA_integer_]

    # Remove the columns that are no longer needed from the two tables
    tab[, (flag_cols) := NULL]
    rnd_cols <- names(tab)[names(tab) %like% "_ase_rnd$"]
    tab[, (rnd_cols) := NULL]
    tab_rounded[, (rnd_cols) := NULL]

    # Remove the flag columns in which no flag is set from the rounded table
    flag_counts <- melt(tab_rounded[, lapply(.SD, \(.) sum(. != "")), .SDcols = flag_cols],
                        measure.vars = flag_cols, variable.factor = FALSE)
    empty_flag_cols <- flag_counts[value == 0L, variable]
    tab_rounded[, (empty_flag_cols) := NULL]

    # Reorder the columns of the rounded table so that the flag columns appear immediately after
    # the columns to which they apply
    nonempty_flag_cols <- setdiff(flag_cols, empty_flag_cols)
    for (flag_col in nonempty_flag_cols) {
      cols <- names(tab_rounded)
      setcolorder(tab_rounded,
                  c(cols[1:(which(cols == gsub("_flag$", "", flag_col)) + 1L)], flag_col))
    }

    tab_rounded
  }


# Round inequality statistics (Gini coefficients and redistributive effects)
round_ineq_stats <-
  function(tab, group_col = "age_bin5", gini_round = 2L) {
    tab_rounded <- copy(tab)

    num_cols <- setdiff(names(tab), c(group_col, "N"))

    tab_rounded[, (num_cols) := lapply(.SD, \(.) round(., gini_round)), .SDcols = num_cols]

    tab_rounded
  }
