## Calculate estimates of average incomes and fiscal components, Gini coefficients, and
## redistributive effects at the family level.
## Average incomes and fiscal components are estimated both under mOECD equivalisation, with the
## individual as the unit of analysis, and unequivalised, with the family as the unit of analysis.
## Gini coefficients and redistributive effects are estimated only under mOECD equivalisation, with
## the individual as the unit of analysis.


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
fis_age[, PrincipalAge := max(P_Attributes_PrincipalEarner_F * P_Attributes_Age), by=.(F_ID)]
fis_age <- make_age_bins(fis_age, "PrincipalAge", 5L, 80L)


# Calculate totals and means - mOECD equivalised values
mkt_inc <- totals_and_means_with_ase(fis_age, "F_Income_Market_FIS", "mkt_inc",
                                     equiv_col = "F_equiv_factor_mOECD", include_pop = TRUE)
inc_sup <- totals_and_means_with_ase(fis_age, "F_Income_Support_FIS", "inc_sup",
                                     equiv_col = "F_equiv_factor_mOECD")
dir_tax <- totals_and_means_with_ase(fis_age, "F_DTax_FIS", "dir_tax",
                                     equiv_col = "F_equiv_factor_mOECD")
disp_inc <- totals_and_means_with_ase(fis_age, "F_Income_Disposable_FIS", "disp_inc",
                                      equiv_col = "F_equiv_factor_mOECD")
ind_tax <- totals_and_means_with_ase(fis_age, "F_ITax_FIS", "ind_tax",
                                     equiv_col = "F_equiv_factor_mOECD")
inkind <- totals_and_means_with_ase(fis_age, "F_InKind_FIS", "in_kind",
                                    equiv_col = "F_equiv_factor_mOECD")
fin_inc <- totals_and_means_with_ase(fis_age, "F_Income_Final_FIS", "fin_inc",
                                     equiv_col = "F_equiv_factor_mOECD")
fis_imp <- totals_and_means_with_ase(fis_age, "F_Fiscal_Impact_FIS", "fis_imp",
                                     equiv_col = "F_equiv_factor_mOECD")

family_level_moecd_equiv_fiscal_cpts <-
  mkt_inc[inc_sup, on = .(age_bin5, N)
  ][dir_tax, on = .(age_bin5, N)
  ][disp_inc, on = .(age_bin5, N)
  ][ind_tax, on = .(age_bin5, N)
  ][inkind, on = .(age_bin5, N)
  ][fin_inc, on = .(age_bin5, N)
  ][fis_imp, on = .(age_bin5, N)]


# Calculate Ginis and redistributive effects - mOECD equivalised values
gini_m <- ginis_with_ase(fis_age, "F_Income_Market_FIS", "gini_m",
                         equiv_col = "F_equiv_factor_mOECD")
gini_d <- ginis_with_ase(fis_age, "F_Income_Disposable_FIS", "gini_d",
                         equiv_col = "F_equiv_factor_mOECD")
gini_f <- ginis_with_ase(fis_age, "F_Income_Final_FIS", "gini_f",
                         equiv_col = "F_equiv_factor_mOECD")

family_level_moecd_equiv_ginis <-
  gini_m[gini_d, on = .(age_bin5, N)
  ][gini_f, on = .(age_bin5, N)]

re_md <-
  redist_effects_with_ase(fis_age, "F_Income_Market_FIS", "F_Income_Disposable_FIS", "re_md",
                          equiv_col = "F_equiv_factor_mOECD")
re_df <-
  redist_effects_with_ase(fis_age, "F_Income_Disposable_FIS", "F_Income_Final_FIS", "re_df",
                          equiv_col = "F_equiv_factor_mOECD")

family_level_moecd_equiv_redist_effects <- re_md[re_df, on = .(age_bin5, N)]


# Calculate totals and means - per family values
mkt_inc <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                     "F_Income_Market_FIS", "mkt_inc", include_pop = TRUE)
inc_sup <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                     "F_Income_Support_FIS", "inc_sup")
dir_tax <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                     "F_DTax_FIS", "dir_tax")
disp_inc <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                      "F_Income_Disposable_FIS", "disp_inc")
ind_tax <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                     "F_ITax_FIS", "ind_tax")
inkind <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                    "F_InKind_FIS", "in_kind")
fin_inc <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                     "F_Income_Final_FIS", "fin_inc")
fis_imp <- totals_and_means_with_ase(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                                     "F_Fiscal_Impact_FIS", "fis_imp")

family_level_per_family_fiscal_cpts <-
  mkt_inc[inc_sup, on = .(age_bin5, N)
  ][dir_tax, on = .(age_bin5, N)
  ][disp_inc, on = .(age_bin5, N)
  ][ind_tax, on = .(age_bin5, N)
  ][inkind, on = .(age_bin5, N)
  ][fin_inc, on = .(age_bin5, N)
  ][fis_imp, on = .(age_bin5, N)]


# Round
family_level_moecd_equiv_fiscal_cpts_rounded <-
  round_totals_and_means(family_level_moecd_equiv_fiscal_cpts,
                         dont_suppress = c("total_fis_imp", "mean_fis_imp"))

family_level_moecd_equiv_ginis_rounded <- round_ineq_stats(family_level_moecd_equiv_ginis)

family_level_moecd_equiv_redist_effects_rounded <-
  round_ineq_stats(family_level_moecd_equiv_redist_effects)

family_level_per_family_fiscal_cpts_rounded <-
  round_totals_and_means(family_level_per_family_fiscal_cpts,
                         dont_suppress = c("total_fis_imp", "mean_fis_imp"))


# Output
fwrite(family_level_moecd_equiv_fiscal_cpts,
       file.path(OUTPUT_DIR, "family_level_moecd_equiv_fiscal_cpts.csv"))
fwrite(family_level_moecd_equiv_fiscal_cpts_rounded,
       file.path(OUTPUT_DIR, "family_level_moecd_equiv_fiscal_cpts_rounded.csv"))

fwrite(family_level_moecd_equiv_ginis,
       file.path(OUTPUT_DIR, "family_level_moecd_equiv_ginis.csv"))
fwrite(family_level_moecd_equiv_ginis_rounded,
       file.path(OUTPUT_DIR, "family_level_moecd_equiv_ginis_rounded.csv"))

fwrite(family_level_moecd_equiv_redist_effects,
       file.path(OUTPUT_DIR, "family_level_moecd_equiv_redist_effects.csv"))
fwrite(family_level_moecd_equiv_redist_effects_rounded,
       file.path(OUTPUT_DIR, "family_level_moecd_equiv_redist_effects_rounded.csv"))

fwrite(family_level_per_family_fiscal_cpts,
       file.path(OUTPUT_DIR, "family_level_per_family_fiscal_cpts.csv"))
fwrite(family_level_per_family_fiscal_cpts_rounded,
       file.path(OUTPUT_DIR, "family_level_per_family_fiscal_cpts_rounded.csv"))
