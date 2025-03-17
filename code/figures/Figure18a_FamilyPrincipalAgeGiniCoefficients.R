library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure18a_FamilyPrincipalAgeGiniCoefficients.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Gini coefficient - Market income",
                                "Gini coefficient - Disposable income",
                                "Gini coefficient - Final income") &
                 `Sharing assumption` == "Family",
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

famgini_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s-\\s(.*)", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  famgini_coefficient = Estimate,
                  famgini_coefficient_ase = `Sampling error`)]

famgini_plot_data[, income_type := factor(income_type,
                                          levels = c("Market income",
                                                     "Disposable income",
                                                     "Final income"))]

famgini_plot_data[, age := factor(age, levels = unique(age))]
famgini_age_labels <- famgini_plot_data[, levels(age)]

setorder(famgini_plot_data, income_type, age)

famgini_plot_data[, x := as.numeric(age)]
famgini_plot_data[income_type == "Market income", x := x - 0.1]
famgini_plot_data[income_type == "Disposable income", x := x + 0.0]
famgini_plot_data[income_type == "Final income", x := x + 0.1]

famgini_plot_labels_panel <-
  famgini_plot_data[age == "15-19" & income_type == "Market income",
                    .(income_type, famgini_coefficient, age, x)]

famgini_plot_labels_panel[, label := "(a)"]
famgini_plot_labels_panel[, famgini_coefficient := 0.98 * (85 - 15) + 15]

p <-
  ggplot(famgini_plot_data,
         aes(x = x, y = famgini_coefficient,
             colour = income_type, shape = income_type)) +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = famgini_coefficient - famgini_coefficient_ase,
                    ymax = famgini_coefficient + famgini_coefficient_ase),
                width = 0,
                show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(famgini_age_labels), labels = famgini_age_labels) +
  scale_y_continuous(limits = c(10, 85), breaks = seq(20, 85, 20)) +
  labs(x = "Age group (family principal earner)",
       y = "Gini coefficient") +
  scale_colour_manual(values = fi_palette) +
  ggtitle("Gini coefficients") +
  geom_text(data = famgini_plot_labels_panel, nudge_x = 0, nudge_y = 0,
            label = famgini_plot_labels_panel$label, colour = "black", size = 5) +
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
