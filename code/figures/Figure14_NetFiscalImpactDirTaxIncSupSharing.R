library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure14_NetFiscalImpactDirTaxIncSupSharing.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic %in% c("Average Net Fiscal Impact", "Average Income support",
                                 "Average Direct taxes", "Average Indirect taxes") &
                  `Sharing assumption` == "Intra-family mOECD") |
               (Statistic %in% c("Average Education spending", "Average Health spending") &
                  `Sharing assumption` == "No sharing"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`, Note)]

nfishr_data_long <-
  data_subset[, .(quantity = Statistic,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`,
                  note = Note)]

nfishr_data_long[note == "Suppressed", amount := 0]
nfishr_data_long[, note := NULL]

nfishr_data_long[quantity %like% "^Average", quantity := gsub("Average", "Mean", quantity)]
nfishr_data_long[quantity %like% "Net", quantity := gsub("Net\\s", "", quantity)]

nfishr_data_long[, quantity := tolower(gsub("\\s", "_", quantity))]

nfishr_data_long <- rbind(nfishr_data_long[, .(quantity, age, amount)],
                          nfishr_data_long[quantity == "mean_fiscal_impact",
                                           .(quantity = "mean_fiscal_impact_ase",
                                             age,
                                             amount = amount_ase)])

nfishr_data_long[, age := factor(age, levels = unique(age))]

nfishr_data_wide <- dcast(nfishr_data_long, age ~ quantity)

nfishr_plot_data <-
  melt(nfishr_data_wide[, .(age,
                            income_support = mean_income_support,
                            inkind_benefits = mean_education_spending + mean_health_spending,
                            direct_tax = -mean_direct_taxes,
                            indirect_tax = -mean_indirect_taxes,
                            fiscal_impact = mean_fiscal_impact,
                            fiscal_impact_ase = mean_fiscal_impact_ase)],
       id.vars = c("age", "fiscal_impact", "fiscal_impact_ase"),
       variable.factor = FALSE)

nfishr_plot_data[variable != "income_support",
                 `:=`(fiscal_impact = NA_real_, fiscal_impact_ase = NA_real_)]

nfishr_plot_data[, variable := factor(variable,
                                      levels = c("inkind_benefits", "income_support",
                                                 "indirect_tax", "direct_tax"))]

setorder(nfishr_plot_data, variable, age)

nfishr_age_labels <- nfishr_plot_data[, levels(age)]

p <-
  ggplot(nfishr_plot_data, aes(x = as.integer(age), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_point(aes(x = as.integer(age), y = fiscal_impact, colour = "black"), size = 2) +
  geom_errorbar(aes(ymin = fiscal_impact - fiscal_impact_ase,
                    ymax = fiscal_impact + fiscal_impact_ase),
                width = 0.2) +
  scale_x_continuous(breaks = 1:length(nfishr_age_labels), labels = nfishr_age_labels) +
  scale_y_continuous(limits = c(-2.2e4, 4.2e4), breaks = seq(-2e4, 4e4, 2e4),
                     labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average benefit or tax ($2019)") +
  guides(colour = guide_legend(order = 1),
         fill = guide_legend(order = 2, override.aes = list(shape = NA))) +
  scale_fill_manual(name = "",
                    values = fi_palette,
                    breaks = c("inkind_benefits", "income_support",
                               "direct_tax", "indirect_tax"),
                    labels = c("In-kind benefits", "Income support",
                               "Direct taxes", "Indirect taxes")) +
  scale_colour_manual(values = "black", labels = "Net fiscal impact") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
