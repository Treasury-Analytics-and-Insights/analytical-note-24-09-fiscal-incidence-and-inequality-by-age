## Calculate estimates of the labour force participation rates for individuals in each age group in
## each of four family roles.


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


# Label individuals by family role
fis_age[F_Nondependents == 1L & P_Attributes_PrincipalEarner_F == 1L, case := "Single"]
fis_age[F_Nondependents == 2L & P_Attributes_PrincipalEarner_F == 1L, case := "Couple-principal"]
fis_age[F_Nondependents == 2L & P_Attributes_Partner_of_PrincipalEarner_F == 1L,
        case := "Couple-partner"]
fis_age[P_Attributes_Dependent == 1L, case := "Dependent"]

# Define labour force status indicators
fis_age[, KnownLFS := 1L * (P_Attributes_LFS %in% c("Employed", "Unemployed", "NIL"))]
fis_age[, ILF := 1L * (P_Attributes_LFS %in% c("Employed", "Unemployed"))]


# Counts and participation rates by replicate
labour_force_part_rates_by_replicate <-
  fis_age[age_bin5 %notin% c("[0,5)", "[5,10)", "[10,15)"),
          .(KnownLFS_N = sum(KnownLFS), KnownLFS_pop = sum(weight * KnownLFS),
            ILF_N = sum(ILF), ILF_pop = sum(weight * ILF),
            LFPR = sum(weight * ILF) / sum(weight * KnownLFS)),
          by = .(age_bin5, case, replicate)]


# Point estimates and sampling errors
labour_force_part_rates <-
  labour_force_part_rates_by_replicate[, .(ILF_N = first(ILF_N),
                                           ILF_pop = first(ILF_pop),
                                           ILF_pop_ase = calc_ase(ILF_pop),
                                           KnownLFS_N = first(KnownLFS_N),
                                           KnownLFS_pop = first(KnownLFS_pop),
                                           KnownLFS_pop_ase = calc_ase(KnownLFS_pop),
                                           LFPR = first(LFPR),
                                           LFPR_ase = calc_ase(LFPR),
                                           LFPR_ase_rnd = calc_ase(round(ILF_pop, -3L) /
                                                                     round(KnownLFS_pop, -3L))),
                                       by = .(case, age_bin5)]

labour_force_part_rates[, case := factor(case,
                                         levels = c("Single", "Couple-principal",
                                                    "Couple-partner", "Dependent"))]
setorder(labour_force_part_rates, case, age_bin5)


# Rounding and suppression
labour_force_part_rates_rounded <- copy(labour_force_part_rates)

for (col in c("ILF_pop", "KnownLFS_pop", "LFPR")) {
  labour_force_part_rates_rounded <- set_rse_flags(labour_force_part_rates_rounded, col)
}

labour_force_part_rates_rounded <-
  set_pop_flags(labour_force_part_rates_rounded, "ILF_N", "ILF_pop")
labour_force_part_rates_rounded <-
  set_pop_flags(labour_force_part_rates_rounded, "KnownLFS_N", "KnownLFS_pop")

labour_force_part_rates_rounded[, `:=`(ILF_pop = round(ILF_pop, -3L),
                                       ILF_pop_ase = round(ILF_pop_ase, -3L))]

labour_force_part_rates_rounded[, `:=`(KnownLFS_pop = round(KnownLFS_pop, -3L),
                                       KnownLFS_pop_ase = round(KnownLFS_pop_ase, -3L))]

labour_force_part_rates_rounded[, `:=`(LFPR = round(ILF_pop / KnownLFS_pop, 3L),
                                       LFPR_ase = round(LFPR_ase_rnd, 3L),
                                       LFPR_ase_rnd = NULL)]
labour_force_part_rates[, LFPR_ase_rnd := NULL]

labour_force_part_rates_rounded <-
  set_rse_flags(labour_force_part_rates_rounded, "LFPR", flag_suffix = "flag2")

labour_force_part_rates_rounded <- combine_flags(labour_force_part_rates_rounded, "LFPR")

labour_force_part_rates_rounded[KnownLFS_pop_flag == "S",
                                `:=`(KnownLFS_N = NA_integer_, KnownLFS_pop = NA_real_,
                                     KnownLFS_pop_ase = NA_real_, ILF_pop_flag = "S")]

labour_force_part_rates_rounded[ILF_pop_flag == "S",
                                `:=`(ILF_N = NA_integer_, ILF_pop = NA_real_,
                                     ILF_pop_ase = NA_real_)]

labour_force_part_rates_rounded[ILF_pop_flag == "S" | KnownLFS_pop_flag == "S", LFPR_flag := "S"]

labour_force_part_rates_rounded[LFPR_flag == "S", `:=`(LFPR = NA_real_, LFPR_ase = NA_real_)]

col_suffixes <- c("N", "pop", "pop_ase", "pop_flag")
setcolorder(labour_force_part_rates_rounded,
            c("case", "age_bin5",
              paste(rep(c("ILF", "KnownLFS"), each = length(col_suffixes)),
                    col_suffixes, sep = "_"),
              "LFPR", "LFPR_ase", "LFPR_flag"))


# Output
fwrite(labour_force_part_rates,
       file.path(OUTPUT_DIR, "labour_force_part_rates.csv"))
fwrite(labour_force_part_rates_rounded,
       file.path(OUTPUT_DIR, "labour_force_part_rates_rounded.csv"))
