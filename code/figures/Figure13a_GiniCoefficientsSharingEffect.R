library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure13a_GiniCoefficientsSharingEffect.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic == "Gini coefficient - Market income" &
                  `Sharing assumption` == "No sharing") |
               (Statistic == "Gini coefficient - Disposable income" &
                  `Sharing assumption` %in% c("No sharing", "Intra-family mOECD")),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`)]

ginishrefct_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s-\\s(.*)\\s.*", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  gini_coefficient = Estimate,
                  gini_coefficient_ase = `Sampling error`)]

ginishrefct_plot_data[income_type == "Market", income_and_sharing_case := "Market"]
ginishrefct_plot_data[income_type == "Disposable" & sharing_assumption == "No sharing",
                      income_and_sharing_case := "Disposable - No sharing"]
ginishrefct_plot_data[income_type == "Disposable" & sharing_assumption == "Intra-family mOECD",
                      income_and_sharing_case := "Disposable - Intra-family mOECD"]
ginishrefct_plot_data[, `:=`(income_type = NULL, sharing_assumption = NULL)]

ginishrefct_plot_data[, income_and_sharing_case :=
                          factor(income_and_sharing_case,
                                 levels = c("Market",
                                            "Disposable - No sharing",
                                            "Disposable - Intra-family mOECD"))]

ginishrefct_plot_data[, age := factor(age, levels = unique(age))]
ginishrefct_age_labels <- ginishrefct_plot_data[, levels(age)]

setorder(ginishrefct_plot_data, income_and_sharing_case, age)

ginishrefct_plot_data[, x := as.numeric(age)]
ginishrefct_plot_data[income_and_sharing_case == "Market", x := x - 0.1]
ginishrefct_plot_data[income_and_sharing_case == "Disposable - No sharing", x := x + 0.0]
ginishrefct_plot_data[income_and_sharing_case == "Disposable - Intra-family mOECD", x := x + 0.1]

ginishrefct_plot_legend_labels <- c("Market\nincome",
                                    sprintf("Disposable income\n\u2014 No sharing"),
                                    sprintf("Disposable income\n\u2014 Intra-fam. mOECD"))

ginishrefct_plot_labels_panel <-
  ginishrefct_plot_data[age == "0-4" & income_and_sharing_case == "Market",
                        .(income_and_sharing_case, gini_coefficient, age, x)]

ginishrefct_plot_labels_panel[, label := "(a)"]
ginishrefct_plot_labels_panel[, gini_coefficient := 0.98 * (85 - 15) + 15]

p <-
  ggplot(ginishrefct_plot_data,
         aes(x = x, y = gini_coefficient,
             colour = income_and_sharing_case, shape = income_and_sharing_case)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = gini_coefficient - gini_coefficient_ase,
                    ymax = gini_coefficient + gini_coefficient_ase),
                width = 0, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(ginishrefct_age_labels), labels = ginishrefct_age_labels) +
  scale_y_continuous(limits = c(15, 85), breaks = seq(20, 85, 20)) +
  labs(x = "Age group",
       y = "Gini coefficient") +
  scale_colour_manual(values = fi_palette, labels = ginishrefct_plot_legend_labels) +
  scale_shape_discrete(labels = ginishrefct_plot_legend_labels) +
  ggtitle("Gini coefficients") +
  geom_text(data = ginishrefct_plot_labels_panel, nudge_x = 0, nudge_y = 0,
            label = ginishrefct_plot_labels_panel$label, colour = "black", size = 5) +
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
