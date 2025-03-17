## Calculate estimates of supplementary quantities that appear in the footnotes:
## The average difference in ages between the two partners in a couple.
## The distribution of age differences between partners in a couple in terms of the number of
## five-year age bins separating them.
## The average numbers of dependent children in families with such children, disaggregated by
## the number of parents in the family.
## The distribution of household sizes in terms of the numbers of families households comprise.


# Source path definitions and analysis functions
source("fis_age_common.R")

# Load data
fis_age_all <- fread(file.path(DATA_DIR, "fis_age_all.csv"))
weights <- fread(file.path(WEIGHTS_DIR, WEIGHTS_FILE))

# Select intra-family mOECD sharing records
fis_age <-
  fis_age_all[sharing_assumption == "Intra-family mOECD", .SD, .SDcols = !"sharing_assumption"]

# Attach replicate weights
fis_age <- attach_replicate_weights(fis_age, weights)

# Define age bins
fis_age <- make_age_bins(fis_age, "P_Attributes_Age", 5L, 80L)


# Differences in couples' ages: mean and median in years
couple_ages_by_replicate <-
  dcast(fis_age[F_Nondependents == 2L & P_Attributes_Dependent == 0L,
                .(replicate, F_ID, weight, P_Attributes_PrincipalEarner_F, P_Attributes_Age)],
        replicate + F_ID + weight ~ P_Attributes_PrincipalEarner_F,
        value.var = "P_Attributes_Age")

couple_age_differences_by_replicate <-
  couple_ages_by_replicate[, .(replicate, F_ID, weight, age_diff = abs(`0` - `1`), idx = 1L)]

couple_age_diffs_mean <-
  totals_and_means_with_ase(couple_age_differences_by_replicate,
                            "age_diff", "age_diff", round_tot = -3L,
                            include_pop = TRUE, group_col = "idx")
couple_age_diffs_mean[, idx := NULL]

couple_age_diffs_median <-
  couple_age_differences_by_replicate[, .(median_age_diff =
                                            Hmisc::wtd.quantile(age_diff, prob = 0.5,
                                                                weights = weight)),
                                      by = .(replicate)
                                      ][, .(median_age_diff = first(median_age_diff),
                                            median_age_diff_ase = calc_ase(median_age_diff))]

couple_age_diffs <- cbind(couple_age_diffs_mean, couple_age_diffs_median)


# Difference in couples' ages: number of 5-year age bins
couple_age_bins_by_replicate <-
  dcast(fis_age[F_Nondependents == 2L & P_Attributes_Dependent == 0L,
                .(replicate, F_ID, weight, P_Attributes_PrincipalEarner_F, age_bin5)],
        replicate + F_ID + weight ~ P_Attributes_PrincipalEarner_F,
        value.var = "age_bin5")

couple_age_bin_differences_by_replicate <-
  couple_age_bins_by_replicate[, .(replicate, F_ID, weight,
                                   age_bin_diff = pmin(abs(as.integer(`0`) -
                                                             as.integer(`1`)), 4L))]

setorder(couple_age_bin_differences_by_replicate, replicate, age_bin_diff)

couple_age_bin_differences_by_replicate[, age_bin_diff := as.character(age_bin_diff)]
couple_age_bin_differences_by_replicate[age_bin_diff == "4", age_bin_diff := "4 or more"]

age_bin_difference_counts_by_replicate <-
  rollup(couple_age_bin_differences_by_replicate, j = list(.N, pop = sum(weight)),
         by = c("replicate", "age_bin_diff")
         )[!is.na(replicate)]

age_bin_difference_counts_by_replicate[, frac := pop / sum(pop * is.na(age_bin_diff)),
                                       by = .(replicate)]
age_bin_difference_counts_by_replicate[, frac_rnd := round(pop, -3L) /
                                         round(sum(pop * is.na(age_bin_diff)), -3L),
                                       by = .(replicate)]

couple_age_bin_diffs <-
  age_bin_difference_counts_by_replicate[,
                                         .(N = first(N), pop = first(pop), pop_ase = calc_ase(pop),
                                           frac = first(frac), frac_ase = calc_ase(frac),
                                           frac_ase_rnd = calc_ase(frac_rnd)),
                                         by = .(age_bin_diff)]

couple_age_bin_diffs[is.na(age_bin_diff), age_bin_diff := "All"]


# Children per family
child_counts_by_replicate <-
  fis_age[F_FamilyType %like% "with dependent$",
          .(weight = first(weight), Ndeps = sum(P_Attributes_Dependent)),
          by = .(replicate, F_ID, F_FamilyType)]

children_per_family_by_replicate <-
  rollup(child_counts_by_replicate,
         j = list(.N, pop = sum(weight),
                  total_children = sum(weight * Ndeps),
                  mean_children = Hmisc::wtd.mean(Ndeps, weight = weight),
                  median_children = Hmisc::wtd.quantile(Ndeps, prob = 0.5, weight = weight)),
         by = c("replicate", "F_FamilyType")
         )[!is.na(replicate)]

children_per_family <-
  children_per_family_by_replicate[, .(N = first(N),
                                       pop = first(pop),
                                       pop_ase = calc_ase(pop),
                                       total_children = first(total_children),
                                       total_children_ase = calc_ase(total_children),
                                       mean_children = first(mean_children),
                                       mean_children_ase = calc_ase(mean_children),
                                       mean_children_ase_rnd = calc_ase(round(total_children, -3L) /
                                                                          round(pop, -3L)),
                                       median_children = first(median_children),
                                       median_children_ase = calc_ase(median_children)),
                                   by = .(famtype = F_FamilyType)]

children_per_family[is.na(famtype), famtype := "All"]


# Families per household
family_counts_by_replicate <-
  fis_age[, .(Nfam = pmin(uniqueN(F_ID), 4L), weight = first(weight)), by = .(H_ID, replicate)]

setorder(family_counts_by_replicate, replicate, Nfam)

family_counts_by_replicate[, Nfam := as.character(Nfam)]
family_counts_by_replicate[Nfam == "4", Nfam := "4 or more"]

families_per_household_by_replicate <-
  rollup(family_counts_by_replicate, j = list(.N, pop = sum(weight)),
         by = c("replicate", "Nfam")
         )[!is.na(replicate)]

families_per_household_by_replicate[, frac := pop / sum(pop * is.na(Nfam)), by = .(replicate)]
families_per_household_by_replicate[, frac_rnd := round(pop, -3L) /
                                                    round(sum(pop * is.na(Nfam)), -3L),
                                    by = .(replicate)]

families_per_household <-
  families_per_household_by_replicate[, .(N = first(N), pop = first(pop),
                                          pop_ase = calc_ase(pop),
                                          frac = first(frac),
                                          frac_ase = calc_ase(frac),
                                          frac_ase_rnd = calc_ase(frac_rnd)),
                                      by = .(Nfam)]

families_per_household[is.na(Nfam), Nfam := "All"]


# Round couple age differences
couple_age_diffs_rounded <- copy(couple_age_diffs)

couple_age_diffs_rounded[, `:=`(pop = round(pop, -3L),
                                     pop_ase = round(pop_ase, -3L),
                                     total_age_diff = round(total_age_diff, -3L),
                                     total_age_diff_ase = round(total_age_diff_ase, -3L),
                                     median_age_diff = round(median_age_diff, 1L),
                                     median_age_diff_ase = round(median_age_diff_ase, 1L))]

couple_age_diffs_rounded[, `:=`(mean_age_diff = round(total_age_diff / pop, 1L),
                                     mean_age_diff_ase = round(mean_age_diff_ase_rnd, 1L),
                                     mean_age_diff_ase_rnd = NULL)]

couple_age_diffs[, mean_age_diff_ase_rnd := NULL]

# NB - No flagging or suppression necessary
checkmate::assert(couple_age_diffs[, sum(N < 6L)] == 0L,
                  couple_age_diffs[, sum(pop < 3000)] == 0L,
                  couple_age_diffs[, sum(pop_ase / pop >= 0.21)] == 0L,
                  couple_age_diffs[, sum(total_age_diff_ase / total_age_diff >= 0.21)] == 0L,
                  couple_age_diffs[, sum(mean_age_diff_ase / mean_age_diff >= 0.21)] == 0L,
                  couple_age_diffs[, sum(median_age_diff_ase / median_age_diff >= 0.21)] == 0L,
                  couple_age_diffs_rounded[, sum(mean_age_diff_ase /
                                                   mean_age_diff >= 0.21)] == 0L,
                  couple_age_diffs_rounded[, sum(median_age_diff_ase /
                                                   median_age_diff >= 0.21)] == 0L,
                  combine = "and")


# Round couple age-bin differences
couple_age_bin_diffs[, `:=`(pop_flag = "", frac_flag = "")]
couple_age_bin_diffs[pop_ase / pop >= 0.21, pop_flag := "*"]
couple_age_bin_diffs[frac_ase / frac >= 0.21, frac_flag := "*"]

couple_age_bin_diffs_rounded <- copy(couple_age_bin_diffs)

couple_age_bin_diffs_rounded[, `:=`(pop = round(pop, -3L),
                                    pop_ase = round(pop_ase, -3L))]

couple_age_bin_diffs_rounded[, `:=`(frac = round(pop / sum((age_bin_diff == "All") * pop), 3L),
                                    frac_ase = round(frac_ase_rnd, 3L),
                                    frac_ase_rnd = NULL)]

couple_age_bin_diffs_rounded[pop_ase / pop >= 0.21, pop_flag := "*"]
couple_age_bin_diffs_rounded[frac_ase / frac >= 0.21, frac_flag := "*"]

setcolorder(couple_age_bin_diffs_rounded,
            c("age_bin_diff", "N", "pop", "pop_ase", "pop_flag", "frac", "frac_ase", "frac_flag"))

couple_age_bin_diffs[, `:=`(frac_ase_rnd = NULL, pop_flag = NULL, frac_flag = NULL)]

# NB - No suppression necessary
checkmate::assert(couple_age_bin_diffs[, sum(N < 6L)] == 0L,
                  couple_age_bin_diffs[, sum(pop < 3000)] == 0L,
                  couple_age_bin_diffs[, sum(pop_ase / pop > 0.5)] == 0L,
                  couple_age_bin_diffs[, sum(frac_ase / frac > 0.5)] == 0L,
                  couple_age_bin_diffs_rounded[, sum(frac_ase / frac > 0.5)] == 0L,
                  combine = "and")


# Round children per family
children_per_family_rounded <- copy(children_per_family)

children_per_family_rounded[, `:=`(pop = round(pop, -3L), pop_ase = round(pop_ase, -3L),
                                   total_children = round(total_children, -3L),
                                   total_children_ase = round(total_children_ase, -3L))]

children_per_family_rounded[, `:=`(mean_children = round(total_children / pop, 2L),
                                   mean_children_ase = round(mean_children_ase_rnd, 2L),
                                   mean_children_ase_rnd = NULL,
                                   median_children_ase = round(median_children_ase, 2L))]

children_per_family_rounded[, median_children_flag := ""]
children_per_family_rounded[median_children_ase / median_children >= 0.21,
                            median_children_flag := "*"]

children_per_family[, mean_children_ase_rnd := NULL]

# NB - No other flagging or suppression necessary
checkmate::assert(children_per_family[, sum(N < 6L)] == 0L,
                  children_per_family[, sum(pop < 3000)] == 0L,
                  children_per_family[, sum(pop_ase / pop >= 0.21)] == 0L,
                  children_per_family[, sum(total_children_ase / total_children >= 0.21)] == 0L,
                  children_per_family[, sum(mean_children_ase / mean_children >= 0.21)] == 0L,
                  children_per_family[, sum(median_children_ase / median_children > 0.5)] == 0L,
                  children_per_family_rounded[, sum(mean_children_ase /
                                                      mean_children >= 0.21)] == 0L,
                  children_per_family_rounded[, sum(median_children_ase /
                                                      median_children > 0.5)] == 0L,
                  combine = "and")


# Round families per household
families_per_household_rounded <- copy(families_per_household)
families_per_household_rounded[, `:=`(pop = round(pop, -3L), pop_ase = round(pop_ase, -3L))]
families_per_household_rounded[, `:=`(frac = round(pop / sum((Nfam == "All") * pop), 3L),
                                      frac_ase = round(frac_ase_rnd, 3L),
                                      frac_ase_rnd = NULL)]
families_per_household[, frac_ase_rnd := NULL]

# NB - No flagging or suppression necessary
checkmate::assert(families_per_household[, sum(N < 6L)] == 0L,
                  families_per_household[, sum(pop < 3000)] == 0L,
                  families_per_household[, sum(pop_ase / pop >= 0.21)] == 0L,
                  families_per_household[, sum(frac_ase / frac >= 0.21)] == 0L,
                  families_per_household_rounded[, sum(frac_ase / frac >= 0.21)] == 0L,
                  combine = "and")


# Output
fwrite(couple_age_diffs,
       file.path(OUTPUT_DIR, "couple_age_diffs.csv"))
fwrite(couple_age_diffs_rounded,
       file.path(OUTPUT_DIR, "couple_age_diffs_rounded.csv"))

fwrite(couple_age_bin_diffs,
       file.path(OUTPUT_DIR, "couple_age_bin_diffs.csv"))
fwrite(couple_age_bin_diffs_rounded,
       file.path(OUTPUT_DIR, "couple_age_bin_diffs_rounded.csv"))

fwrite(children_per_family,
       file.path(OUTPUT_DIR, "children_per_family.csv"))
fwrite(children_per_family_rounded,
       file.path(OUTPUT_DIR, "children_per_family_rounded.csv"))

fwrite(families_per_household,
       file.path(OUTPUT_DIR, "families_per_household.csv"))
fwrite(families_per_household_rounded,
       file.path(OUTPUT_DIR, "families_per_household_rounded.csv"))
