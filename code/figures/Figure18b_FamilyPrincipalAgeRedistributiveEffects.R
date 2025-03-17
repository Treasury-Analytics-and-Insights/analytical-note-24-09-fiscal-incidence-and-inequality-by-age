library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figufamredist_filename <- "../../figures/Figure18b_FamilyPrincipalAgeRedistributiveEffects.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Redistributive effect - Market to disposable",
                                "Redistributive effect - Disposable to final") &
                 `Sharing assumption` == "Family",
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

famredist_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s-\\s(.*)", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  redistributive_effect = Estimate,
                  redistributive_effect_ase = `Sampling error`)]

famredist_plot_data[, income_type := factor(income_type,
                                            levels = c("Market to disposable",
                                                       "Disposable to final"))]

famredist_plot_data[, age := factor(age, levels = unique(age))]
famredist_age_labels <- famredist_plot_data[, levels(age)]

setorder(famredist_plot_data, income_type, age)

famredist_plot_data[, x := as.numeric(age)]
famredist_plot_data[income_type == "Market to disposable", x := x - 0.05]
famredist_plot_data[income_type == "Disposable to final", x := x + 0.05]

famredist_plot_legend_labels <- c(sprintf("Market \u2192 Disposable"),
                                  sprintf("Disposable \u2192 Final"))

famredist_plot_labels_panel <-
  famredist_plot_data[age == "15-19" & income_type == "Market to disposable",
                      .(income_type, redistributive_effect, age, x)]

famredist_plot_labels_panel[, label := "(b)"]
famredist_plot_labels_panel[, redistributive_effect := 0.98 * 60]

p <-
  ggplot(famredist_plot_data,
         aes(x = x, y = redistributive_effect,
             colour = income_type, shape = income_type)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = redistributive_effect - redistributive_effect_ase,
                    ymax = redistributive_effect + redistributive_effect_ase),
                width = 0,
                show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(famredist_age_labels), labels = famredist_age_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20)) +
  labs(x = "Age group (family principal earner)",
       y = "Redistributive effect") +
  scale_colour_manual(values = fi_palette, labels = famredist_plot_legend_labels) +
  scale_shape_discrete(labels = famredist_plot_legend_labels) +
  ggtitle("Redistributive effects") +
  geom_text(data = famredist_plot_labels_panel, nudge_x = 0, nudge_y = 0,
            label = famredist_plot_labels_panel$label, colour = "black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figufamredist_filename,
       width = 6, height = 4, units = "in", dpi = 600, type = "cairo")
