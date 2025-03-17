## Calculate estimates of average incomes for individuals in four distinct family types under the
## assumption of intra-family mOECD sharing.


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

age_bins <- fis_age[, .(age_bin5 = unique(age_bin5))][order(age_bin5)]


# Calculate fiscal components for each family type
famtype_intrafam_moecd_fiscal_cpts <- data.table()

famtypes <- fis_age[, unique(F_FamilyType)]

for (famtype in famtypes) {
  fis_age_famtype <- fis_age[F_FamilyType == famtype]

  mkt_inc <-
    totals_and_means_with_ase(fis_age_famtype, "P_Income_Market_FIS", "mkt_inc", include_pop = TRUE)
  disp_inc <-
    totals_and_means_with_ase(fis_age_famtype, "P_Income_Disposable_FIS_shared", "disp_inc")
  fin_inc <-
    totals_and_means_with_ase(fis_age_famtype, "P_Income_Final_FIS", "fin_inc")

  intrafam_moecd_fiscal_cpts <-
    mkt_inc[disp_inc, on = .(age_bin5, N)
    ][fin_inc, on = .(age_bin5, N)]

  # Fill cells for unobserved family type - age combinations with zeros
  min_age_bin_i <- intrafam_moecd_fiscal_cpts[, min(as.integer(age_bin5))]
  max_age_bin_i <- age_bins[, max(as.integer(age_bin5))]
  intrafam_moecd_fiscal_cpts <-
    intrafam_moecd_fiscal_cpts[age_bins[min_age_bin_i:max_age_bin_i], on = .(age_bin5)]
  intrafam_moecd_fiscal_cpts[is.na(intrafam_moecd_fiscal_cpts)] <- 0L

  intrafam_moecd_fiscal_cpts[, famtype := famtype]
  setcolorder(intrafam_moecd_fiscal_cpts, "famtype")

  famtype_intrafam_moecd_fiscal_cpts <-
    rbind(famtype_intrafam_moecd_fiscal_cpts, intrafam_moecd_fiscal_cpts)
}


# Round
famtype_intrafam_moecd_fiscal_cpts_rounded <-
  round_totals_and_means(famtype_intrafam_moecd_fiscal_cpts)


# Output
fwrite(famtype_intrafam_moecd_fiscal_cpts,
       file.path(OUTPUT_DIR, "famtype_intrafam_moecd_fiscal_cpts.csv"))
fwrite(famtype_intrafam_moecd_fiscal_cpts_rounded,
       file.path(OUTPUT_DIR, "famtype_intrafam_moecd_fiscal_cpts_rounded.csv"))
