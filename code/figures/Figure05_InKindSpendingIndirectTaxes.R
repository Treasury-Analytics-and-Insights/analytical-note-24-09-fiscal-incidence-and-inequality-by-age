library(data.table)
library(ggplot2)
library(ggh4x)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure05_InKindSpendingIndirectTaxes.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic %in% c("Average Education spending", "Average Health spending") &
                  `Sharing assumption` == "No sharing") |
               (Statistic == "Average Indirect taxes" &
                  `Sharing assumption` == "Intra-family mOECD"),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`,
                 Note)]

eduhlthitax_plot_data <-
  data_subset[, .(income_or_tax_type = gsub(".*\\s(\\w+\\s\\w+)", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`,
                  note = Note)]

eduhlthitax_plot_data[sharing_assumption == "No sharing", panel := "Education and health spending"]
eduhlthitax_plot_data[sharing_assumption == "Intra-family mOECD", panel := "Indirect taxes"]

eduhlthitax_plot_data[, panel := factor(panel, levels = c("Education and health spending",
                                                          "Indirect taxes"))]

eduhlthitax_plot_data[, income_or_tax_type :=
                          factor(income_or_tax_type,
                                 levels = c("Education spending",
                                            "Health spending",
                                            "Indirect taxes"))]

eduhlthitax_plot_data[, age := factor(age, levels = unique(age))]
eduhlthitax_age_labels <- eduhlthitax_plot_data[, levels(age)]

setorder(eduhlthitax_plot_data, panel, income_or_tax_type, age)

eduhlthitax_plot_labels <- eduhlthitax_plot_data[, .(income_or_tax_type, panel, amount, age, note)]
eduhlthitax_plot_labels[note == "Suppressed", label := "S"]
eduhlthitax_plot_labels[, `:=`(amount = 0, note = NULL)]

eduhlthitax_plot_labels_edu <- eduhlthitax_plot_labels[income_or_tax_type == "Education spending"]

eduhlthitax_plot_labels_sup <- copy(eduhlthitax_plot_labels_edu[1])
eduhlthitax_plot_labels_sup[, label := "S - Suppressed"]

eduhlthitax_plot_labels_panel <-
  eduhlthitax_plot_data[age == "0-4" & income_or_tax_type != "Education spending",
                        .(income_or_tax_type, sharing_assumption, amount, age, panel)]

eduhlthitax_plot_labels_panel[, label := paste0("(", letters[1:2], ")")]
eduhlthitax_plot_labels_panel[, amount := 0.98 * c(2.1e4, 6.3e3)]

p <-
  ggplot(eduhlthitax_plot_data,
         aes(x = as.integer(age), y = amount,
             colour = income_or_tax_type, shape = income_or_tax_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = amount - amount_ase, ymax = amount + amount_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(eduhlthitax_age_labels), labels = eduhlthitax_age_labels) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average amount ($2019)") +
  scale_colour_manual(values = fi_palette) +
  geom_text(data = eduhlthitax_plot_labels_edu, nudge_x = 0, nudge_y = 0,
            label = eduhlthitax_plot_labels_edu$label, colour = fi_palette[1], size = 5) +
  geom_text(data = eduhlthitax_plot_labels_sup, nudge_x = 12.5, nudge_y = 17500,
            label = eduhlthitax_plot_labels_sup$label, colour = "black", size = 5) +
  geom_text(data = eduhlthitax_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = eduhlthitax_plot_labels_panel$label, colour = "black", size = 5) +
  facet_wrap(~ panel, nrow = 1L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 20, hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white", linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facetted_pos_scales(y = list(panel == "Education and health spending" ~
                                 scale_y_continuous(limits = c(0, 2.1e4),
                                                    breaks = seq(0, 2e4, 5e3),
                                                    labels = scales::dollar_format()),
                               panel == "Indirect taxes" ~
                                 scale_y_continuous(limits = c(0, 6.3e3),
                                                    breaks = seq(0, 6e3, 1.5e3),
                                                    labels = scales::dollar_format())))

ggsave(p, filename = figure_filename,
       width = 13, height = 4.8, units = "in", dpi = 600, type = "cairo")
