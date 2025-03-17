## Construct tables with the shared fiscal variables and incomes for each unit record under various
## sharing assumptions.


library(data.table)


# Define path and file names
FIS_DIR <- "" # Directory containing the fiscal incidence study unit record data
FIS_FILE <- "" # Filename of the fiscal incidence study unit record data
WEIGHTS_DIR <- "" # Directory containing the TAWA replicate weights
WEIGHTS_FILE <- "" # Filename of the TAWA replicate weights
HES_DIR <- "" # Directory containing the HES 2018/19 PersonDem table
HES_PERSON_FILE <- "" # Filename of the HES 2018/19 PersonDem table
DATA_DIR <- "../data"


# Load fiscal incidence unit record data
fis <- fread(file.path(FIS_DIR, FIS_FILE))


# Load replicate weights
weights <- fread(file.path(WEIGHTS_DIR, WEIGHTS_FILE))


# Load labour force status codes from HES table
hes_person <-
  fread(file.path(HES_DIR, HES_PERSON_FILE))[,.(snz_hes_uid, snz_hes_hhld_uid, DVLFStatus)]


# Attach labour force status codes to fiscal incidence records
fis <- hes_person[fis, on = .(snz_hes_hhld_uid, snz_hes_uid)]


# Identify each individual's labour force status from the HES codes
fis[, P_Attributes_LFS := "LFSMissing"]
fis[DVLFStatus == 1L, P_Attributes_LFS := "Employed"]
fis[DVLFStatus == 2L, P_Attributes_LFS := "Unemployed"]
fis[DVLFStatus == 3L, P_Attributes_LFS := "NIL"]
fis[DVLFStatus == 99L, P_Attributes_LFS := "LFSUnidentified"]


# Identify families with no non-dependents
dep_only_F_IDs <- fis[, .(N_nondep = sum(!P_Attributes_Dependent),
                          N_15plus = sum(P_Attributes_Age >= 15L)),
                      by = .(F_ID)
                      ][N_nondep == 0L, .(F_ID, N_15plus)]


# Where a family has no non-dependents and no members aged 15+, append its members to a family in
# the same household
fis[dep_only_F_IDs[N_15plus == 0L], on = .(F_ID), F_ID := NA_integer_]
fis[, F_ID := zoo::na.locf(zoo::na.locf(F_ID), fromLast = TRUE), by = .(H_ID)]


# Where a family has no non-dependents but does have at least one member aged 15+, identify the
# eldest family member as the parent or caregiver
fis[dep_only_F_IDs[N_15plus > 0L], on = .(F_ID), age_of_oldest_f := max(P_Attributes_Age),
    by = .(F_ID)]
fis[, first_oldest_f := 0L]
fis[P_Attributes_Age == age_of_oldest_f, first_oldest_f := 1L * (P_ID == first(P_ID)),
    by = .(F_ID)]
fis[dep_only_F_IDs[N_15plus > 0L], on = .(F_ID),
    P_Attributes_Dependent := 1L * (first_oldest_f == 0L)]
fis[, `:=`(age_of_oldest_f = NULL, first_oldest_f = NULL)]


# Identify the principal earner in each family
fis[, max_adult_earnings_f := max((!P_Attributes_Dependent) * abs(P_Income_Market_FIS)),
    by = .(F_ID)]
fis[, P_Attributes_PrincipalEarner_F := 0L]
fis[(abs(P_Income_Market_FIS) == max_adult_earnings_f) & (P_Attributes_Dependent == 0L),
    P_Attributes_PrincipalEarner_F := 1L * (P_ID == first(P_ID)),
    by = .(F_ID)]
fis[, max_adult_earnings_f := NULL]

# Identify the partner of the family principal earner, if one is present
fis[, P_Attributes_Partner_of_PrincipalEarner_F :=
        (1L - P_Attributes_Dependent) * (1L - P_Attributes_PrincipalEarner_F)]

# Define the household principal earner
fis[, max_adult_earnings_h := max((P_Attributes_Age >= 14L) * abs(P_Income_Market_FIS)),
    by = .(H_ID)]
fis[, P_Attributes_PrincipalEarner_H := 0L]
fis[(abs(P_Income_Market_FIS) == max_adult_earnings_h) & (P_Attributes_Age >= 14L),
    P_Attributes_PrincipalEarner_H := 1L * (P_ID == first(P_ID)),
    by = .(H_ID)]
fis[, max_adult_earnings_h := NULL]


# Categorise each family by the numbers of parents and dependents in it
fis[, F_Nondependents := sum(!P_Attributes_Dependent), by = .(F_ID)]
fis[, F_Dependents := sum(P_Attributes_Dependent), by = .(F_ID)]
fis[F_Nondependents == 1L & F_Dependents >= 1L, F_FamilyType := "Non-couple with dependent"]
fis[F_Nondependents == 1L & F_Dependents == 0L, F_FamilyType := "Non-couple without dependent"]
fis[F_Nondependents == 2L & F_Dependents >= 1L, F_FamilyType := "Couple with dependent"]
fis[F_Nondependents == 2L & F_Dependents == 0L, F_FamilyType := "Couple without dependent"]


# Redistribute any Working for Families tax credits uniformly across the parents in each family
fis[, P_WorkingforFamilies_FIS_orig := P_WorkingforFamilies_FIS]
fis[, F_WorkingforFamilies_FIS := sum(P_WorkingforFamilies_FIS), by = .(F_ID)]
fis[P_Attributes_Dependent == 0L,
    P_WorkingforFamilies_FIS := F_WorkingforFamilies_FIS / F_Nondependents]
fis[P_Attributes_Dependent == 1L, P_WorkingforFamilies_FIS := 0]


# Adjust income support and disposable income amounts to reflect the redistribution of Working for
# Families tax credits
fis[, P_Income_Support_FIS_orig := P_Income_Support_FIS]
fis[, P_Income_Support_FIS := P_Income_Support_FIS_orig - P_WorkingforFamilies_FIS_orig +
                                P_WorkingforFamilies_FIS]
fis[, P_Income_Disposable_FIS := P_Income_Disposable_FIS - P_Income_Support_FIS_orig +
                                   P_Income_Support_FIS]


# Weights of individuals in each household under intra-household mOECD sharing
fis[, PH_weight := 1 * (P_Attributes_PrincipalEarner_H == 1L)]
fis[P_Attributes_PrincipalEarner_H == 0L & P_Attributes_Age >= 14L, PH_weight := 0.5]
fis[P_Attributes_Age < 14L, PH_weight := 0.3]


# Equivalence factor of each household
fis[, H_equiv_factor_mOECD := sum(PH_weight), by = .(H_ID)]


# Weights of individuals in each family under intra-family mOECD sharing
fis[, PF_weight := 1 * (P_Attributes_PrincipalEarner_F == 1L)]
fis[P_Attributes_PrincipalEarner_F == 0L & P_Attributes_Dependent == 0L, PF_weight := 0.5]
fis[P_Attributes_Dependent == 1L, PF_weight := 0.3]


# Equivalence factor of each family
fis[, F_equiv_factor_mOECD := sum(PF_weight), by = .(F_ID)]


# Sum of equivalence factors of the families in each household
fis[, FH_equiv_factor_mOECD := sum(PF_weight), by = .(H_ID)]


# Family-level aggregates
fis[, F_Income_Market_FIS := sum(P_Income_Market_FIS), by = .(F_ID)]
fis[, F_Income_Support_FIS := sum(P_Income_Support_FIS), by = .(F_ID)]
fis[, F_DTax_FIS := sum(P_DTax_FIS), by = .(F_ID)]
fis[, F_Income_Disposable_FIS := sum(P_Income_Disposable_FIS), by = .(F_ID)]
fis[, F_Education_FIS := sum(P_Education_FIS), by = .(F_ID)]
fis[, F_Health_FIS := sum(P_Health_FIS), by = .(F_ID)]
fis[, F_InKind_FIS := F_Health_FIS + F_Education_FIS]


# In any household containing a family with a negative disposable income, share indirect taxes
# in proportion to the absolute values of family disposable incomes
fis[, first_person_f := 1L * (P_ID == first(P_ID)), by = .(F_ID)]

neg_F_disp_H_IDs <- fis[(F_Income_Disposable_FIS < 0), .(H_ID = unique(H_ID))]

fis[neg_F_disp_H_IDs, on = .(H_ID),
    F_ind_tax_share := abs(F_Income_Disposable_FIS) /
                         sum(abs(F_Income_Disposable_FIS) * first_person_f),
    by = .(H_ID)]


# In any household in which all families have zero disposable income, share indirect taxes in
# proportion to family mOECD equivalence factors
no_nonzero_F_Disp_Inc_H_IDs <-
  fis[, .(H_has_nonzero_F_Disp_Incs = 1L * sum(F_Income_Disposable_FIS != 0)), by = .(H_ID)
      ][H_has_nonzero_F_Disp_Incs == 0L, .(H_ID)]

fis[no_nonzero_F_Disp_Inc_H_IDs, on = .(H_ID),
    F_ind_tax_share := F_equiv_factor_mOECD / FH_equiv_factor_mOECD]


# In all other households, share indirect taxes in proportion to the disposable income shares
fis[!rbind(neg_F_disp_H_IDs, no_nonzero_F_Disp_Inc_H_IDs), on = .(H_ID),
    F_ind_tax_share := F_Income_Disposable_FIS / H_Income_Disposable_FIS]


# Remove alcohol and tobacco excises amounts from any households with no 18+ members, and
# redistribute these amounts over the remaining households
fis[, H_No18plus := 1L * (sum(P_Attributes_Age >= 18L) == 0L), by = .(H_ID)]

fis[, first_person_h := 1L * (P_ID == first(P_ID)), by = .(H_ID)]

fis <- weights[, .(H_ID, weight = P_Weight_2019TaxYearIntegrated_Rep0)
               ][fis, on = .(H_ID)]

fis[, `:=`(total_IndirectTax_Excise_Alcohol = sum(first_person_h * weight *
                                                    H_IndirectTax_Excise_Alcohol),
           total_IndirectTax_Excise_Tobacco = sum(first_person_h * weight *
                                                    H_IndirectTax_Excise_Tobacco))]

fis[H_No18plus == 0L,
       `:=`(H_IndirectTax_Excise_Alcohol = H_IndirectTax_Excise_Alcohol /
                                             sum(first_person_h * weight *
                                                   H_IndirectTax_Excise_Alcohol) *
                                             total_IndirectTax_Excise_Alcohol,
            H_IndirectTax_Excise_Tobacco = H_IndirectTax_Excise_Tobacco /
                                             sum(first_person_h * weight *
                                                   H_IndirectTax_Excise_Tobacco) *
                                             total_IndirectTax_Excise_Tobacco)]

fis[H_No18plus == 1L, `:=`(H_IndirectTax_Excise_Alcohol = 0, H_IndirectTax_Excise_Tobacco = 0)]

fis[, `:=`(total_IndirectTax_Excise_Alcohol = NULL, total_IndirectTax_Excise_Tobacco = NULL,
           weight = NULL, first_person_h = NULL)]


# Exclude any family with no 18+ members from being assigned alcohol and tobacco excises, and
# adjust the shares of these excises assigned to all other families in the household
fis[, F_No18plus := 1L * (sum(P_Attributes_Age >= 18L) == 0L), by = .(F_ID)]

fis[F_No18plus == 1L, F_ind_tax_share_alc_tob := 0]

fis[F_No18plus == 0L,
    F_ind_tax_share_alc_tob := F_ind_tax_share / sum(first_person_f * F_ind_tax_share),
    by = .(H_ID)]
fis[, first_person_f := NULL]


# Apply household-level to family-level sharing of indirect taxes
fis[, `:=`(F_ITax_Excise_Alcohol = F_ind_tax_share_alc_tob * H_IndirectTax_Excise_Alcohol,
           F_ITax_Excise_Tobacco = F_ind_tax_share_alc_tob * H_IndirectTax_Excise_Tobacco)]

fis[, `:=`(F_ITax_GST = F_ind_tax_share * H_IndirectTax_GST,
           F_ITax_Excise_Petrol = F_ind_tax_share * H_IndirectTax_Excise_Petrol)]

fis[, F_ITax_FIS := F_ITax_Excise_Alcohol + F_ITax_Excise_Petrol +
                      F_ITax_Excise_Tobacco + F_ITax_GST]

# Family-level variables that involve indirect taxes
fis[, F_Income_Final_FIS := F_Income_Disposable_FIS + F_InKind_FIS - F_ITax_FIS]

fis[, F_Fiscal_Impact_FIS := F_Income_Support_FIS + F_InKind_FIS - F_DTax_FIS - F_ITax_FIS]


# Calculate household and family headcounts
fis[, H_People := .N, by = .(H_ID)]
fis[, F_People := .N, by = .(F_ID)]


# Initialise output data table
fis_age_all <- data.table()


# Intra-family mOECD sharing
fis_age <- copy(fis)
fis_age[, sharing_assumption := "Intra-family mOECD"]

fis_age[, P_Income_Support_FIS_shared :=
            PF_weight * F_Income_Support_FIS / F_equiv_factor_mOECD]
fis_age[, P_DTax_FIS_shared :=
            PF_weight * F_DTax_FIS / F_equiv_factor_mOECD]
fis_age[, P_Income_Disposable_FIS_shared :=
            PF_weight * F_Income_Disposable_FIS / F_equiv_factor_mOECD]

fis_age[, P_ITax_GST_shared := PF_weight * F_ITax_GST / F_equiv_factor_mOECD]
fis_age[, P_ITax_Excise_Petrol_shared :=
            PF_weight * F_ITax_Excise_Petrol / F_equiv_factor_mOECD]

fis_age[P_Attributes_Age >= 18L,
        P_ITax_Excise_Alcohol_shared := PF_weight / sum(PF_weight) *
                                          F_ITax_Excise_Alcohol,
        by = .(F_ID)]
fis_age[P_Attributes_Age < 18L, P_ITax_Excise_Alcohol_shared := 0]

fis_age[P_Attributes_Age >= 18L,
        P_ITax_Excise_Tobacco_shared := PF_weight / sum(PF_weight) *
                                          F_ITax_Excise_Tobacco,
        by = .(F_ID)]
fis_age[P_Attributes_Age < 18L, P_ITax_Excise_Tobacco_shared := 0]

fis_age[, P_ITax_FIS_shared :=
            P_ITax_GST_shared +
            P_ITax_Excise_Alcohol_shared +
            P_ITax_Excise_Petrol_shared +
            P_ITax_Excise_Tobacco_shared]

fis_age_all <- rbind(fis_age_all, fis_age)


# Intra-household mOECD sharing
fis_age <- copy(fis)
fis_age[, sharing_assumption := "Intra-household mOECD"]

fis_age[, P_Income_Support_FIS_shared :=
            PH_weight * H_Income_Support_FIS / H_equiv_factor_mOECD]
fis_age[, P_DTax_FIS_shared :=
            PH_weight * H_DTax_FIS / H_equiv_factor_mOECD]
fis_age[, P_Income_Disposable_FIS_shared :=
            PH_weight * H_Income_Disposable_FIS / H_equiv_factor_mOECD]

fis_age[, P_ITax_GST_shared := PH_weight * H_IndirectTax_GST / H_equiv_factor_mOECD]
fis_age[, P_ITax_Excise_Petrol_shared :=
            PH_weight * H_IndirectTax_Excise_Petrol / H_equiv_factor_mOECD]

fis_age[P_Attributes_Age >= 18L,
        P_ITax_Excise_Alcohol_shared := PH_weight / sum(PH_weight) *
                                          H_IndirectTax_Excise_Alcohol,
        by = .(H_ID)]
fis_age[P_Attributes_Age < 18L, P_ITax_Excise_Alcohol_shared := 0]

fis_age[P_Attributes_Age >= 18L,
        P_ITax_Excise_Tobacco_shared := PH_weight / sum(PH_weight) *
                                          H_IndirectTax_Excise_Tobacco,
        by = .(H_ID)]
fis_age[P_Attributes_Age < 18L, P_ITax_Excise_Tobacco_shared := 0]

fis_age[, P_ITax_FIS_shared :=
            P_ITax_GST_shared +
            P_ITax_Excise_Alcohol_shared +
            P_ITax_Excise_Petrol_shared +
            P_ITax_Excise_Tobacco_shared]

fis_age_all <- rbind(fis_age_all, fis_age)


# Intra-family equal sharing
fis_age <- copy(fis)
fis_age[, sharing_assumption := "Intra-family equal"]

fis_age[, P_Income_Support_FIS_shared := F_Income_Support_FIS / F_People]
fis_age[, P_DTax_FIS_shared := F_DTax_FIS / F_People]
fis_age[, P_Income_Disposable_FIS_shared := F_Income_Disposable_FIS / F_People]

fis_age[, P_ITax_GST_shared := F_ITax_GST / F_People]
fis_age[, P_ITax_Excise_Petrol_shared := F_ITax_Excise_Petrol / F_People]

fis_age[P_Attributes_Age >= 18L,
        P_ITax_Excise_Alcohol_shared := 1 / .N * F_ITax_Excise_Alcohol,
        by = .(F_ID)]
fis_age[P_Attributes_Age < 18L, P_ITax_Excise_Alcohol_shared := 0]

fis_age[P_Attributes_Age >= 18L,
        P_ITax_Excise_Tobacco_shared := 1 / .N * F_ITax_Excise_Tobacco,
        by = .(F_ID)]
fis_age[P_Attributes_Age < 18L, P_ITax_Excise_Tobacco_shared := 0]

fis_age[, P_ITax_FIS_shared :=
            P_ITax_GST_shared +
            P_ITax_Excise_Alcohol_shared +
            P_ITax_Excise_Petrol_shared +
            P_ITax_Excise_Tobacco_shared]

fis_age_all <- rbind(fis_age_all, fis_age)


# No sharing
fis_age <- copy(fis)
fis_age[, sharing_assumption := "No sharing"]

fis_age[, P_Income_Support_FIS_shared := P_Income_Support_FIS]
fis_age[, P_DTax_FIS_shared := P_DTax_FIS]
fis_age[, P_Income_Disposable_FIS_shared := P_Income_Disposable_FIS]

fis_age[, `:=`(P_ITax_GST_shared = NA_real_,
               P_ITax_Excise_Alcohol_shared = NA_real_,
               P_ITax_Excise_Petrol_shared = NA_real_,
               P_ITax_Excise_Tobacco_shared = NA_real_,
               P_ITax_FIS_shared = NA_real_)]

fis_age_all <- rbind(fis_age_all, fis_age)


# Variables derived from shared quantities
fis_age_all[, P_Income_Final_FIS :=
                P_Income_Disposable_FIS_shared +
                P_Income_InKind_FIS -
                P_ITax_FIS_shared]

fis_age_all[, P_Fiscal_Impact_FIS :=
                P_Income_Support_FIS +
                P_Income_InKind_FIS -
                P_DTax_FIS -
                P_ITax_FIS_shared]

fis_age_all[, P_Fiscal_Impact_FIS_shared :=
                P_Income_Support_FIS_shared +
                P_Income_InKind_FIS -
                P_DTax_FIS_shared -
                P_ITax_FIS_shared]


# Write
setcolorder(fis_age_all, "sharing_assumption")
fwrite(fis_age_all, file.path(DATA_DIR, "fis_age_all.csv"))
