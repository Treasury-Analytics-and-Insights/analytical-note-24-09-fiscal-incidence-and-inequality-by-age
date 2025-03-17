## Calculate estimates of average incomes and fiscal amounts, Gini coefficients, and redistributive
## effects for individuals under the assumption of intra-family equal sharing.


# Source path definitions and analysis functions
source("fis_age_common.R")

# Load data
fis_age_all <- fread(file.path(DATA_DIR, "fis_age_all.csv"))
weights <- fread(file.path(WEIGHTS_DIR, WEIGHTS_FILE))

# Select intra-family equal sharing records
fis_age <-
  fis_age_all[sharing_assumption == "Intra-family equal", .SD, .SDcols = ! "sharing_assumption"]

# Attach replicate weights
fis_age <- attach_replicate_weights(fis_age, weights)

# Define age bins
fis_age <- make_age_bins(fis_age, "P_Attributes_Age", 5L, 80L)


# Calculate totals and means
inc_sup <- totals_and_means_with_ase(fis_age, "P_Income_Support_FIS_shared", "inc_sup",
                                     include_pop = TRUE)
dir_tax <- totals_and_means_with_ase(fis_age, "P_DTax_FIS_shared", "dir_tax")
disp_inc <- totals_and_means_with_ase(fis_age, "P_Income_Disposable_FIS_shared", "disp_inc")
ind_tax <- totals_and_means_with_ase(fis_age, "P_ITax_FIS_shared", "ind_tax")
fin_inc <- totals_and_means_with_ase(fis_age, "P_Income_Final_FIS", "fin_inc")

intrafam_equal_fiscal_cpts <-
  inc_sup[dir_tax, on = .(age_bin5, N)
  ][disp_inc, on = .(age_bin5, N)
  ][ind_tax, on = .(age_bin5, N)
  ][fin_inc, on = .(age_bin5, N)]


# Calculate Ginis and redistributive effects
gini_m <- ginis_with_ase(fis_age, "P_Income_Market_FIS", "gini_m")
gini_d <- ginis_with_ase(fis_age, "P_Income_Disposable_FIS_shared", "gini_d")
gini_f <- ginis_with_ase(fis_age, "P_Income_Final_FIS", "gini_f")

intrafam_equal_ginis <-
  gini_m[gini_d, on = .(age_bin5, N)
  ][gini_f, on = .(age_bin5, N)]

re_md <-
  redist_effects_with_ase(fis_age, "P_Income_Market_FIS", "P_Income_Disposable_FIS_shared", "re_md")
re_df <-
  redist_effects_with_ase(fis_age, "P_Income_Disposable_FIS_shared", "P_Income_Final_FIS", "re_df")

intrafam_equal_redist_effects <- re_md[re_df, on = .(age_bin5, N)]


# Round
intrafam_equal_fiscal_cpts_rounded <- round_totals_and_means(intrafam_equal_fiscal_cpts)
intrafam_equal_ginis_rounded <- round_ineq_stats(intrafam_equal_ginis)
intrafam_equal_redist_effects_rounded <- round_ineq_stats(intrafam_equal_redist_effects)


# Output
fwrite(intrafam_equal_fiscal_cpts,
       file.path(OUTPUT_DIR, "intrafam_equal_fiscal_cpts.csv"))
fwrite(intrafam_equal_fiscal_cpts_rounded,
       file.path(OUTPUT_DIR, "intrafam_equal_fiscal_cpts_rounded.csv"))

fwrite(intrafam_equal_ginis,
       file.path(OUTPUT_DIR, "intrafam_equal_ginis.csv"))
fwrite(intrafam_equal_ginis_rounded,
       file.path(OUTPUT_DIR, "intrafam_equal_ginis_rounded.csv"))

fwrite(intrafam_equal_redist_effects,
       file.path(OUTPUT_DIR, "intrafam_equal_redist_effects.csv"))
fwrite(intrafam_equal_redist_effects_rounded,
       file.path(OUTPUT_DIR, "intrafam_equal_redist_effects_rounded.csv"))
