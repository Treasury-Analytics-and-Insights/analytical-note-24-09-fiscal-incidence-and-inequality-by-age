## Calculate for each age group estimates of the the populations of individuals in each family role,
## individuals belonging to each family type, and families of each type.


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


# Assign family type and role indicators
fis_age[, Principal := P_Attributes_PrincipalEarner_F]
fis_age[, Partner := P_Attributes_Partner_of_PrincipalEarner_F]
fis_age[, Dependent := P_Attributes_Dependent]

fis_age[, Single := 1L * (F_FamilyType == "Non-couple without dependent")]
fis_age[, SolePar := 1L * (F_FamilyType == "Non-couple with dependent")]
fis_age[, Couple := 1L * (F_FamilyType == "Couple without dependent")]
fis_age[, CoupleKids := 1L * (F_FamilyType == "Couple with dependent")]


# Counts tables
individual_and_family_counts <- function(tab, group_col, classification, categories) {
  tab_long <- melt(tab[, .SD, .SDcols = c("replicate", "weight", group_col, categories)],
                   id.vars = c("replicate", "weight", group_col), variable.name = "classification")

  setnames(tab_long, group_col, "group_col")

  pop_tabs <-
    lapply(categories,
           \(.) totals_and_means_with_ase(tab_long[classification == . & value == 1L],
                                          "value", "NULL", group_col = "group_col",
                                          include_pop = TRUE
                                          )[, .SD, .SDcols = c("group_col", "N", "pop", "pop_ase")])

  out <- rbindlist(setNames(pop_tabs, categories), idcol = "classification")

  out[, classification := factor(classification, levels = unique(classification))]

  setcolorder(out, "group_col")

  out <-
    out[CJ(classification = unique(classification),
           group_col = unique(group_col)),
        on = .(classification, group_col)]

  setnames(out, c("classification", "group_col"), c(classification, group_col))

  out[is.na(out)] <- 0L

  out
}


individuals_by_family_role <-
  individual_and_family_counts(fis_age, "age_bin5", "family_role",
                               c("Principal", "Partner", "Dependent"))

individuals_by_family_type <-
  individual_and_family_counts(fis_age, "age_bin5", "family_type",
                               c("Single", "SolePar", "Couple", "CoupleKids"))

families_by_family_type <-
  individual_and_family_counts(fis_age[P_Attributes_PrincipalEarner_F == 1L],
                               "age_bin5", "family_type",
                               c("Single", "SolePar", "Couple", "CoupleKids"))


# Round
individuals_by_family_role_rounded <- round_totals_and_means(individuals_by_family_role)
individuals_by_family_type_rounded <- round_totals_and_means(individuals_by_family_type)
families_by_family_type_rounded <- round_totals_and_means(families_by_family_type)


# Structural zeroes
individuals_by_family_role_rounded[age_bin5 %in% c("[0,5)", "[5,10)", "[10,15)") &
                                     family_role %in% c("Principal", "Partner"),
                                   `:=`(N = 0L, pop = 0, pop_ase = 0, pop_flag = "")]
individuals_by_family_role_rounded[age_bin5 %notin% c("[0,5)", "[5,10)", "[10,15)", "[15,20)") &
                                     family_role == "Dependent",
                                   `:=`(N = 0L, pop = 0, pop_ase = 0, pop_flag = "")]

individuals_by_family_type_rounded[age_bin5 %in% c("[0,5)", "[5,10)", "[10,15)") &
                                     family_type %in% c("Single", "Couple"),
                                   `:=`(N = 0L, pop = 0, pop_ase = 0, pop_flag = "")]


# Pivot to wide
pivot_and_reorder <- function(tab, group_col, classification, categories) {
  measures <- c("N", "pop", "pop_ase", "pop_flag")
  measures <- intersect(measures, names(tab))

  setnames(tab, c(group_col, classification), c("group_col", "classification"))

  tab_wide <- dcast(tab, group_col ~ classification, value.var = measures)

  col_names <- names(tab_wide)
  for (group in categories) {
    col_names <-
      gsub(paste("(.*)", paste0(group, "$"), sep = "_"), paste(group, "\\1", sep = "_"), col_names)
  }
  setnames(tab_wide, col_names)
  setnames(tab_wide, "group_col", group_col)

  new_col_order <-
    c(group_col, paste(rep(categories, each = length(measures)), measures, sep = "_"))
  setcolorder(tab_wide, new_col_order)

  flag_cols <- names(tab_wide)[names(tab_wide) %like% "flag$"]

  if (length(flag_cols) > 0L) {
    drop_cols <- melt(tab_wide[, .SD, .SDcols = flag_cols][, lapply(.SD, \(.) sum(. != ""))],
                      measure.vars = flag_cols, variable.factor = FALSE)[value == 0L, variable]
    tab_wide[, (drop_cols) := NULL]
  }

  tab_wide
}


individuals_by_family_role <-
  pivot_and_reorder(individuals_by_family_role, "age_bin5", "family_role",
                    c("Principal", "Partner", "Dependent"))
individuals_by_family_role_rounded <-
  pivot_and_reorder(individuals_by_family_role_rounded, "age_bin5", "family_role",
                    c("Principal", "Partner", "Dependent"))

individuals_by_family_type <-
  pivot_and_reorder(individuals_by_family_type, "age_bin5", "family_type",
                    c("Single", "SolePar", "Couple", "CoupleKids"))
individuals_by_family_type_rounded <-
  pivot_and_reorder(individuals_by_family_type_rounded, "age_bin5", "family_type",
                    c("Single", "SolePar", "Couple", "CoupleKids"))

families_by_family_type <-
  pivot_and_reorder(families_by_family_type, "age_bin5", "family_type",
                    c("Single", "SolePar", "Couple", "CoupleKids"))
families_by_family_type_rounded <-
  pivot_and_reorder(families_by_family_type_rounded, "age_bin5", "family_type",
                    c("Single", "SolePar", "Couple", "CoupleKids"))


# Output
fwrite(individuals_by_family_role,
       file.path(OUTPUT_DIR, "individuals_by_family_role.csv"))
fwrite(individuals_by_family_role_rounded,
       file.path(OUTPUT_DIR, "individuals_by_family_role_rounded.csv"))

fwrite(individuals_by_family_type,
       file.path(OUTPUT_DIR, "individuals_by_family_type.csv"))
fwrite(individuals_by_family_type_rounded,
       file.path(OUTPUT_DIR, "individuals_by_family_type_rounded.csv"))

fwrite(families_by_family_type,
       file.path(OUTPUT_DIR, "families_by_family_type.csv"))
fwrite(families_by_family_type_rounded,
       file.path(OUTPUT_DIR, "families_by_family_type_rounded.csv"))
