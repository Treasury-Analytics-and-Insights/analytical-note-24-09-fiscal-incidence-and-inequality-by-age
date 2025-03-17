## Calculate estimates of average incomes and fiscal amounts, Gini coefficients, and redistributive
## effects for individuals in the absence of an intra-family or intra-household sharing asssumption.


# Source path definitions and analysis functions
source("fis_age_common.R")

# Load data
fis_age_all <- fread(file.path(DATA_DIR, "fis_age_all.csv"))
weights <- fread(file.path(WEIGHTS_DIR, WEIGHTS_FILE))

# Select no sharing records
fis_age <- fis_age_all[sharing_assumption == "No sharing", .SD, .SDcols = !"sharing_assumption"]

# Attach replicate weights
fis_age <- attach_replicate_weights(fis_age, weights)

# Define age bins
fis_age <- make_age_bins(fis_age, "P_Attributes_Age", 5L, 80L)


# Calculate totals and means
mkt_inc <- totals_and_means_with_ase(fis_age, "P_Income_Market_FIS", "mkt_inc", include_pop = TRUE)
inc_sup <- totals_and_means_with_ase(fis_age, "P_Income_Support_FIS", "inc_sup")
dir_tax <- totals_and_means_with_ase(fis_age, "P_DTax_FIS", "dir_tax")
disp_inc <- totals_and_means_with_ase(fis_age, "P_Income_Disposable_FIS", "disp_inc")
edu <- totals_and_means_with_ase(fis_age, "P_Education_FIS", "edu")
health <- totals_and_means_with_ase(fis_age, "P_Health_FIS", "health")

no_sharing_fiscal_cpts <-
  mkt_inc[inc_sup, on = .(age_bin5, N)
  ][dir_tax, on = .(age_bin5, N)
  ][disp_inc, on = .(age_bin5, N)
  ][edu, on = .(age_bin5, N)
  ][health, on = .(age_bin5, N)]


# Calculate totals and means - income support components
super <- totals_and_means_with_ase(fis_age, "P_Super_Amount_Gross_FIS", "super", include_pop = TRUE)
inc_rep_wa <- totals_and_means_with_ase(fis_age, "P_Income_Replacement_WA_FIS", "inc_rep_wa")
housing <- totals_and_means_with_ase(fis_age, "P_Housing_FIS", "housing")
wff <- totals_and_means_with_ase(fis_age, "P_WorkingforFamilies_FIS", "wff")
other <- totals_and_means_with_ase(fis_age, "P_Other_Income_Support_FIS", "other")

no_sharing_inc_sup_cpts <-
  super[inc_rep_wa, on = .(age_bin5, N)
  ][housing, on = .(age_bin5, N)
  ][wff, on = .(age_bin5, N)
  ][other, on = .(age_bin5, N)]


# Calculate totals and means - components of other income support
wep <- totals_and_means_with_ase(fis_age, "P_Benefits_WinterEnergy_FIS", "wep", include_pop = TRUE)
othnt <- totals_and_means_with_ase(fis_age, "P_Benefits_Totals_OtherNonTaxable_FIS", "othnt")
ietc <- totals_and_means_with_ase(fis_age, "P_TaxCredit_IETC", "ietc")
studall <- totals_and_means_with_ase(fis_age, "P_Income_StudentAllowance_FIS", "studall")
ppl <- totals_and_means_with_ase(fis_age, "P_FamilyAssistance_ParentalLeave_Amount_FIS", "ppl")
ypypp <- totals_and_means_with_ase(fis_age, "P_Benefits_YPYPP_Amount_Gross_FIS", "ypypp")

no_sharing_oth_inc_sup_cpts <-
  wep[othnt, on = .(age_bin5, N)
  ][ietc, on = .(age_bin5, N)
  ][studall, on = .(age_bin5, N)
  ][ppl, on = .(age_bin5, N)
  ][ypypp, on = .(age_bin5, N)]


# Calculate Ginis and redistributive effects
gini_m <- ginis_with_ase(fis_age, "P_Income_Market_FIS", "gini_m")
gini_d <- ginis_with_ase(fis_age, "P_Income_Disposable_FIS_shared", "gini_d")

no_sharing_ginis <- gini_m[gini_d, on = .(age_bin5, N)]

re_md <-
  redist_effects_with_ase(fis_age, "P_Income_Market_FIS", "P_Income_Disposable_FIS_shared", "re_md")
no_sharing_redist_effects <- re_md


# Round
no_sharing_fiscal_cpts_rounded <- round_totals_and_means(no_sharing_fiscal_cpts)
no_sharing_inc_sup_cpts_rounded <- round_totals_and_means(no_sharing_inc_sup_cpts)
no_sharing_oth_inc_sup_cpts_rounded <- round_totals_and_means(no_sharing_oth_inc_sup_cpts)
no_sharing_ginis_rounded <- round_ineq_stats(no_sharing_ginis)
no_sharing_redist_effects_rounded <- round_ineq_stats(no_sharing_redist_effects)


# Output
fwrite(no_sharing_fiscal_cpts,
       file.path(OUTPUT_DIR, "no_sharing_fiscal_cpts.csv"))
fwrite(no_sharing_fiscal_cpts_rounded,
       file.path(OUTPUT_DIR, "no_sharing_fiscal_cpts_rounded.csv"))

fwrite(no_sharing_inc_sup_cpts,
       file.path(OUTPUT_DIR, "no_sharing_inc_sup_cpts.csv"))
fwrite(no_sharing_inc_sup_cpts_rounded,
       file.path(OUTPUT_DIR, "no_sharing_inc_sup_cpts_rounded.csv"))

fwrite(no_sharing_oth_inc_sup_cpts,
       file.path(OUTPUT_DIR, "no_sharing_oth_inc_sup_cpts.csv"))
fwrite(no_sharing_oth_inc_sup_cpts_rounded,
       file.path(OUTPUT_DIR, "no_sharing_oth_inc_sup_cpts_rounded.csv"))

fwrite(no_sharing_ginis,
       file.path(OUTPUT_DIR, "no_sharing_ginis.csv"))
fwrite(no_sharing_ginis_rounded,
       file.path(OUTPUT_DIR, "no_sharing_ginis_rounded.csv"))

fwrite(no_sharing_redist_effects,
       file.path(OUTPUT_DIR, "no_sharing_redist_effects.csv"))
fwrite(no_sharing_redist_effects_rounded,
       file.path(OUTPUT_DIR, "no_sharing_redist_effects_rounded.csv"))
