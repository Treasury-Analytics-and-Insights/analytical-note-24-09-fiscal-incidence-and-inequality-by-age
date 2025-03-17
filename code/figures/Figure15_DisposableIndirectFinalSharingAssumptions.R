library(data.table)
library(ggplot2)
library(ggh4x)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure15_DisposableIndirectFinalSharingAssumptions.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Average Disposable income", "Average Indirect taxes",
                                "Average Final income") &
                 `Sharing assumption` %in% c("Intra-family mOECD", "Intra-family equal",
                                             "Intra-household mOECD"),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`)]

dfitaxaltshr_plot_data <-
  data_subset[, .(income_or_tax_type = gsub(".*\\s(\\w+\\s\\w+)", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`)]

dfitaxaltshr_plot_data[, income_or_tax_type :=
                           factor(income_or_tax_type,
                                  levels = c("Disposable income",
                                             "Indirect taxes",
                                             "Final income"))]

dfitaxaltshr_plot_data[, sharing_assumption :=
                           factor(sharing_assumption,
                                  levels = c("Intra-family mOECD",
                                             "Intra-family equal",
                                             "Intra-household mOECD"))]

dfitaxaltshr_plot_data[, age := factor(age, levels = unique(age))]

setorder(dfitaxaltshr_plot_data, income_or_tax_type, sharing_assumption, age)

disindfin_age_labels <- dfitaxaltshr_plot_data[, levels(age)]

dfitaxaltshr_plot_labels_panel <-
  dfitaxaltshr_plot_data[age == "0-4" & sharing_assumption == "Intra-family mOECD",
                         .(income_or_tax_type, sharing_assumption, amount, age)]

dfitaxaltshr_plot_labels_panel[, label := paste0("(", letters[1:3], ")")]
dfitaxaltshr_plot_labels_panel[, amount := 0.98 * c(6e4, 6e3, 6e4)]

p <-
  ggplot(dfitaxaltshr_plot_data,
         aes(x = as.integer(age), y = amount,
             colour = sharing_assumption, shape = sharing_assumption)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = amount - amount_ase, ymax = amount + amount_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1L, length(disindfin_age_labels), by = 2L),
                     labels = disindfin_age_labels[seq(1L, length(disindfin_age_labels),
                                                       by = 2L)]) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average amount ($2019)") +
  scale_colour_manual(values = fi_palette) +
  geom_text(data = dfitaxaltshr_plot_labels_panel, nudge_x = 0, nudge_y = 0,
            label = dfitaxaltshr_plot_labels_panel$label,
            colour = "black", size = 5) +
  facet_wrap(~ income_or_tax_type, nrow = 1L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 16),
        legend.text = element_text(size = 14),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 20, hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white", linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facetted_pos_scales(y = list(income_or_tax_type == "Disposable income" ~
                                 scale_y_continuous(limits = c(0, 6e4), breaks = seq(0, 6e4, 2e4),
                                                    labels = scales::dollar_format()),
                               income_or_tax_type == "Indirect taxes" ~
                                 scale_y_continuous(limits = c(0, 6e3), breaks = seq(0, 6e3, 2e3),
                                                    labels = scales::dollar_format()),
                               income_or_tax_type == "Final income" ~
                                 scale_y_continuous(limits = c(0, 6e4), breaks = seq(0, 6e4, 2e4),
                                                    labels = scales::dollar_format())))

ggsave(p, filename = figure_filename,
       width = 13, height = 4, units = "in", dpi = 600, type = "cairo")
