library(data.table)
library(ggplot2)
library(ggh4x)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure09_FamilyTypesMarketDisposableFinal.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic %in% c("Average Market income - Single-member families",
                                 "Average Market income - Sole-parent families",
                                 "Average Market income - Couple families",
                                 "Average Market income - Couple-with-children families") &
                  `Sharing assumption` == "No sharing") |
               (Statistic %in% c("Average Disposable income - Single-member families",
                                 "Average Disposable income - Sole-parent families",
                                 "Average Disposable income - Couple families",
                                 "Average Disposable income - Couple-with-children families",
                                 "Average Final income - Single-member families",
                                 "Average Final income - Sole-parent families",
                                 "Average Final income - Couple families",
                                 "Average Final income - Couple-with-children families") &
                  `Sharing assumption` == "Intra-family mOECD"),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`,
                 Note)]

famtypesmdf_plot_data <-
  data_subset[, .(income_type = gsub("\\w+\\s(\\w+\\s\\w+).*", "\\1", Statistic),
                  family_type = gsub("\\w+\\s\\w+\\s\\w+\\s-\\s(.*)\\s\\w+", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  income = Estimate,
                  income_ase = `Sampling error`,
                  note = Note)]

famtypesmdf_plot_data[, income_type := factor(income_type,
                                              levels = c("Market income",
                                                         "Disposable income",
                                                         "Final income"))]

famtypesmdf_plot_data[family_type == "Single-member", family_type := "Single"]
famtypesmdf_plot_data[family_type == "Sole-parent", family_type := "Sole parent"]
famtypesmdf_plot_data[family_type == "Couple", family_type := "Couple without dependents"]
famtypesmdf_plot_data[family_type == "Couple-with-children",
                      family_type := "Couple with dependent(s)"]

famtypesmdf_plot_data[, family_type := factor(family_type,
                                              levels = c("Single",
                                                         "Sole parent",
                                                         "Couple without dependents",
                                                         "Couple with dependent(s)"))]

famtypesmdf_plot_data[, age := factor(age, levels = unique(age))]
famtypesmdf_age_labels <- famtypesmdf_plot_data[, levels(age)]

setorder(famtypesmdf_plot_data, income_type, family_type, age)

famtypesmdf_plot_labels <-
  famtypesmdf_plot_data[, .(income_type, family_type, income, age, note)]
famtypesmdf_plot_labels[note == "Suppressed", label := "S"]
famtypesmdf_plot_labels[, `:=`(income = 0, note = NULL)]

famtypesmdf_plot_labels_mkt_s <-
  famtypesmdf_plot_labels[income_type == "Market income" &
                            family_type %in% c("Single", "Sole parent")]
famtypesmdf_plot_labels_mkt_c <-
  famtypesmdf_plot_labels[income_type == "Market income" &
                            family_type %in% c("Couple without dependents",
                                               "Couple with dependent(s)")]
famtypesmdf_plot_labels_dis_s <-
  famtypesmdf_plot_labels[income_type == "Disposable income" &
                            family_type %in% c("Single", "Sole parent")]
famtypesmdf_plot_labels_dis_c <-
  famtypesmdf_plot_labels[income_type == "Disposable income" &
                            family_type %in% c("Couple without dependents",
                                               "Couple with dependent(s)")]
famtypesmdf_plot_labels_fin_s <-
  famtypesmdf_plot_labels[income_type == "Final income" &
                            family_type %in% c("Single", "Sole parent")]
famtypesmdf_plot_labels_fin_c <-
  famtypesmdf_plot_labels[income_type == "Final income" &
                            family_type %in% c("Couple without dependents",
                                               "Couple with dependent(s)")]

famtypesmdf_plot_labels_sup <-
  copy(famtypesmdf_plot_labels_mkt_s[family_type == "Sole parent"][1])
famtypesmdf_plot_labels_sup[, label := "S - Suppressed"]

famtypesmdf_plot_labels_panel <-
  famtypesmdf_plot_data[age == "0-4" & income_type == "Market income",
                        .(income_type, family_type, income, age)]

famtypesmdf_plot_labels_panel[, label := paste0("(", letters[1:4], ")")]
famtypesmdf_plot_labels_panel[, income := 0.98 * c(7.5e4, 7.5e4, 1e5, 1e5)]

p <-
  ggplot(famtypesmdf_plot_data,
         aes(x = as.integer(age), y = income,
             colour = income_type, shape = income_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = income - income_ase, ymax = income + income_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(famtypesmdf_age_labels),
                     labels = famtypesmdf_age_labels) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average income ($2019)") +
  scale_colour_manual(values = fi_palette) +
  geom_text(data = famtypesmdf_plot_labels_mkt_s, nudge_x = 0, nudge_y = 2000,
            label = famtypesmdf_plot_labels_mkt_s$label, colour = fi_palette[1], size = 4) +
  geom_text(data = famtypesmdf_plot_labels_mkt_c, nudge_x = 0, nudge_y = 2660,
            label = famtypesmdf_plot_labels_mkt_c$label, colour = fi_palette[1], size = 4) +
  geom_text(data = famtypesmdf_plot_labels_dis_s, nudge_x = 0, nudge_y = 6000,
            label = famtypesmdf_plot_labels_dis_s$label, colour = fi_palette[2], size = 4) +
  geom_text(data = famtypesmdf_plot_labels_dis_c, nudge_x = 0, nudge_y = 7980,
            label = famtypesmdf_plot_labels_dis_c$label, colour = fi_palette[2], size = 4) +
  geom_text(data = famtypesmdf_plot_labels_fin_s, nudge_x = 0, nudge_y = 10000,
            label = famtypesmdf_plot_labels_fin_s$label, colour = fi_palette[3], size = 4) +
  geom_text(data = famtypesmdf_plot_labels_fin_c, nudge_x = 0, nudge_y = 13300,
            label = famtypesmdf_plot_labels_fin_c$label, colour = fi_palette[3], size = 4) +
  geom_text(data = famtypesmdf_plot_labels_sup, nudge_x = 13.5, nudge_y = 70000,
            label = famtypesmdf_plot_labels_sup$label, colour = "black", size = 5) +
  geom_text(data = famtypesmdf_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = famtypesmdf_plot_labels_panel$label, colour = "black", size = 5) +
  facet_wrap(~ family_type, nrow = 2L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 20, hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white", linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facetted_pos_scales(y = list(family_type == "Single" ~
                                 scale_y_continuous(limits = c(0, 7.5e4),
                                                    breaks = seq(0, 7.5e4, 2.5e4),
                                                    labels = scales::dollar_format()),
                               family_type == "Sole parent" ~
                                 scale_y_continuous(limits = c(0, 7.5e4),
                                                    breaks = seq(0, 7.5e4, 2.5e4),
                                                    labels = scales::dollar_format()),
                               family_type == "Couple without dependents" ~
                                 scale_y_continuous(limits = c(0, 1e5),
                                                    breaks = seq(0, 1e5, 2.5e4),
                                                    labels = scales::dollar_format()),
                               family_type == "Couple with dependent(s)" ~
                                 scale_y_continuous(limits = c(0, 1e5),
                                                    breaks = seq(0, 1e5, 2.5e4),
                                                    labels = scales::dollar_format())))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
