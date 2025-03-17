library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure13b_RedistributiveEffectsSharingEffect.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic == "Redistributive effect - Market to disposable" &
                 `Sharing assumption` %in% c("No sharing", "Intra-family mOECD"),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`)]

redistshrefct_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s-\\s(.*)", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  redistributive_effect = Estimate,
                  redistributive_effect_ase = `Sampling error`)]

redistshrefct_plot_data[income_type == "Market to disposable" & sharing_assumption == "No sharing",
                        income_and_sharing_case := "Market to disposable - No sharing"]
redistshrefct_plot_data[income_type == "Market to disposable" &
                          sharing_assumption == "Intra-family mOECD",
                        income_and_sharing_case := "Market to disposable - Intra-family mOECD"]
redistshrefct_plot_data[, `:=`(income_type = NULL, sharing_assumption = NULL)]

redistshrefct_plot_data[, income_and_sharing_case :=
                            factor(income_and_sharing_case,
                                   levels = c("Market to disposable - No sharing",
                                              "Market to disposable - Intra-family mOECD"))]

redistshrefct_plot_data[, age := factor(age, levels = unique(age))]
redistshrefct_age_labels <- redistshrefct_plot_data[, levels(age)]

setorder(redistshrefct_plot_data, income_and_sharing_case, age)

redistshrefct_plot_data[, x := as.numeric(age)]
redistshrefct_plot_data[income_and_sharing_case == "Market to disposable - No sharing",
                        x := x - 0.05]
redistshrefct_plot_data[income_and_sharing_case == "Market to disposable - Intra-family mOECD",
                        x := x + 0.05]

redistshrefct_plot_legend_labels <- c(sprintf("Market \u2192 Disposable\n\u2014 No sharing"),
                                      sprintf("Market \u2192 Disposable\n\u2014 Intra-fam. mOECD"))

redistshrefct_plot_labels_panel <-
  redistshrefct_plot_data[age == "0-4" &
                            income_and_sharing_case == "Market to disposable - No sharing",
                          .(income_and_sharing_case, redistributive_effect, age, x)]

redistshrefct_plot_labels_panel[, label := "(b)"]
redistshrefct_plot_labels_panel[, redistributive_effect := 0.98 * 60]

p <-
  ggplot(redistshrefct_plot_data,
         aes(x = x, y = redistributive_effect,
             colour = income_and_sharing_case, shape = income_and_sharing_case)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = redistributive_effect - redistributive_effect_ase,
                    ymax = redistributive_effect + redistributive_effect_ase),
                width = 0, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(redistshrefct_age_labels),
                     labels = redistshrefct_age_labels) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20)) +
  labs(x = "Age group",
       y = "Redistributive effect") +
  scale_colour_manual(values = fi_palette, labels = redistshrefct_plot_legend_labels) +
  scale_shape_discrete(labels = redistshrefct_plot_legend_labels) +
  ggtitle("Redistributive effects") +
  geom_text(data = redistshrefct_plot_labels_panel, nudge_x = 0, nudge_y = 0,
            label = redistshrefct_plot_labels_panel$label, colour = "black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 6, height = 4, units = "in", dpi = 600, type = "cairo")
