## Calculate estimates of the rates of receipt of the three core benefits and of the non-qualified
## partner NZ Superannuation payment for individuals in each age group.


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


# Assign benefit receipt indicators
fis_age[, jss := 1L * (P_Benefits_JSS_Amount_Gross_FIS > 0)]
fis_age[, slp := 1L * (P_Benefits_SLP_Amount_Gross_FIS > 0)]
fis_age[, sps := 1L * (P_Benefits_SPS_Amount_Gross_FIS > 0)]
fis_age[, nqp := 1L * (P_Super_Amount_Gross_FIS > 0) * (P_Attributes_Age < 65L)]


# Counts and rates by replicate
benefit_abbrevs <- c("jss", "slp", "sps", "nqp")

benefit_receipt_rates_by_replicate <-
  fis_age[age_bin5 %notin% c("[0,5)", "[5,10)", "[10,15)"),
          .(.N, pop = sum(weight)),
          by = .(age_bin5, replicate)]

for (ben in benefit_abbrevs) {
  ben_receipt_rates_by_replicate <-
    fis_age[age_bin5 %notin% c("[0,5)", "[5,10)", "[10,15)"),
            .(ben_N = sum(get(ben)),
              ben_pop = sum(weight * get(ben)),
              ben_frac = sum(weight * get(ben)) / sum(weight)),
            by = .(age_bin5, replicate)]

  setnames(ben_receipt_rates_by_replicate,
           paste("ben", c("N", "pop", "frac"), sep = "_"),
           paste(ben, c("N", "pop", "frac"), sep = "_"))

  benefit_receipt_rates_by_replicate <-
    benefit_receipt_rates_by_replicate[ben_receipt_rates_by_replicate, on = .(age_bin5, replicate)]
}


# Point estimates and sampling errors
benefit_receipt_rates <-
  benefit_receipt_rates_by_replicate[, .(N = first(N), pop = first(pop), pop_ase = calc_ase(pop)),
                                     by = .(age_bin5)]

col_suffixes <- c("N", "pop", "pop_ase", "frac", "frac_ase", "frac_ase_rnd")

for (ben in benefit_abbrevs) {
  ben_pops <-
    benefit_receipt_rates_by_replicate[, .(ben_N = first(get(paste(ben, "N", sep = "_"))),
                                           ben_pop = first(get(paste(ben, "pop", sep = "_"))),
                                           ben_pop_ase = calc_ase(get(paste(ben, "pop",
                                                                            sep = "_"))),
                                           ben_frac = first(get(paste(ben, "frac", sep = "_"))),
                                           ben_frac_ase = calc_ase(get(paste(ben, "frac",
                                                                             sep = "_"))),
                                           ben_frac_ase_rnd =
                                             calc_ase(round(get(paste(ben, "pop", sep = "_")),
                                                            -3L) /
                                                        round(pop, -3L))),
                                       by = .(age_bin5)]

  setnames(ben_pops,
           paste("ben", col_suffixes, sep = "_"),
           paste(ben, col_suffixes, sep = "_"))

  benefit_receipt_rates <- benefit_receipt_rates[ben_pops, on = .(age_bin5)]
}

setorder(benefit_receipt_rates, age_bin5)

col_suffixes <- col_suffixes[!(col_suffixes %like% "_rnd$")]
setcolorder(benefit_receipt_rates,
            c("age_bin5", "N", "pop", "pop_ase",
              paste(rep(benefit_abbrevs, each = length(col_suffixes)), col_suffixes, sep = "_")))


# Rounding and suppression
benefit_receipt_rates_rounded <- copy(benefit_receipt_rates)

pop_cols <- c("pop", paste(benefit_abbrevs, "pop", sep = "_"))
frac_cols <- paste(benefit_abbrevs, "frac", sep = "_")

for (pop_col in c(pop_cols, frac_cols)) {
  benefit_receipt_rates_rounded <- set_rse_flags(benefit_receipt_rates_rounded, pop_col)
}

samp_cols <- c("N", paste(benefit_abbrevs, "N", sep = "_"))
for (i in seq_along(pop_cols)) {
  benefit_receipt_rates_rounded <-
    set_pop_flags(benefit_receipt_rates_rounded, samp_cols[i], pop_cols[i])
}

pop_and_ase_cols <- c(pop_cols, paste(pop_cols, "ase", sep = "_"))

benefit_receipt_rates_rounded[, (pop_and_ase_cols) := lapply(.SD, \(.) round(., -3L)),
                              .SDcols = pop_and_ase_cols]

benefit_receipt_rates_rounded[, (paste(benefit_abbrevs, "frac", sep = "_")) :=
                                lapply(.SD, \(.) round(. / pop, 3L)),
                              .SDcols = paste(benefit_abbrevs, "pop", sep = "_")]

benefit_receipt_rates_rounded[, (paste(benefit_abbrevs, "frac_ase", sep = "_")) :=
                                lapply(.SD, \(.) round(., 3L)),
                              .SDcols = paste(benefit_abbrevs, "frac_ase_rnd", sep = "_")]

benefit_receipt_rates[, (paste(benefit_abbrevs, "frac_ase_rnd", sep = "_")) := NULL]
benefit_receipt_rates_rounded[, (paste(benefit_abbrevs, "frac_ase_rnd", sep = "_")) := NULL]

for (ben in benefit_abbrevs) {
  benefit_receipt_rates_rounded <- set_rse_flags(benefit_receipt_rates_rounded,
                                                 paste(ben, "frac", sep = "_"),
                                                 flag_suffix = "flag2")
}

for (ben in benefit_abbrevs) {
  benefit_receipt_rates_rounded <-
    combine_flags(benefit_receipt_rates_rounded, paste(ben, "frac", sep = "_"))
}


benefit_receipt_rates_rounded[pop_flag == "S",
                              (paste(benefit_abbrevs, "pop_flag", sep = "_")) := "S"]

for (ben in benefit_abbrevs) {
  benefit_receipt_rates_rounded[get(paste(ben, "pop_flag", sep = "_")) == "S",
                                (paste(ben, "frac_flag", sep = "_")) := "S"]
}

benefit_receipt_rates_rounded[pop_flag == "S",
                              `:=`(N = NA_integer_, pop = NA_real_, pop_ase = NA_real_)]

for (ben in benefit_abbrevs) {
  benefit_receipt_rates_rounded[get(paste(ben, "pop_flag", sep ="_")) == "S",
                                (paste(ben, c("N", "pop", "pop_ase"), sep = "_")) := NA_real_]
benefit_receipt_rates_rounded[get(paste(ben, "frac_flag", sep = "_")) == "S",
                              (paste(ben, c("frac", "frac_ase"), sep = "_")) := NA_real_]
}


col_suffixes <- c("N", "pop", "pop_ase", "pop_flag", "frac", "frac_ase", "frac_flag")
setcolorder(benefit_receipt_rates_rounded,
            c("age_bin5", "N", "pop", "pop_ase",
              paste(rep(benefit_abbrevs, each = length(col_suffixes)), col_suffixes, sep = "_")))

flag_cols <-
  names(benefit_receipt_rates_rounded)[names(benefit_receipt_rates_rounded) %like% "flag$"]

drop_cols <- melt(benefit_receipt_rates_rounded[, .SD, .SDcols = flag_cols
                                                ][, lapply(.SD, \(.) sum(. != ""))],
                  measure.vars = flag_cols,
                  variable.factor = FALSE
                  )[value == 0L, variable]

benefit_receipt_rates_rounded[, (drop_cols) := NULL]


# Structural zeros
benefit_receipt_rates_rounded[age_bin5 %in% c("[65,70)", "[70,75)", "[75,80)", "[80+)"),
                              `:=`(nqp_N = 0L, nqp_pop = 0L, nqp_pop_ase = 0, nqp_pop_flag = "",
                                   nqp_frac = 0, nqp_frac_ase = 0, nqp_frac_flag = "")]


# Output
fwrite(benefit_receipt_rates,
       file.path(OUTPUT_DIR, "benefit_receipt_rates.csv"))
fwrite(benefit_receipt_rates_rounded,
       file.path(OUTPUT_DIR, "benefit_receipt_rates_rounded.csv"))
