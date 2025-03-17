library(data.table)
library(ggplot2)
library(ggh4x)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <-
  "../../figures/Figure16_GiniCoefficientsRedistributiveEffectsSharingAssumptions.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Gini coefficient - Disposable income",
                                "Gini coefficient - Final income",
                                "Redistributive effect - Market to disposable",
                                "Redistributive effect - Disposable to final") &
                 `Sharing assumption` %in% c("Intra-family mOECD", "Intra-family equal",
                                            "Intra-household mOECD"),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`)]

ginirealtshr_plot_data <-
  data_subset[, .(quantity = Statistic,
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  gini_coefficient = Estimate,
                  gini_coefficient_ase = `Sampling error`)]

ginirealtshr_plot_data[, quantity := gsub("-", "\u2013", quantity)]
ginirealtshr_plot_data[, quantity := gsub("coefficient", "coefficients", quantity)]
ginirealtshr_plot_data[, quantity := gsub("effect", "effects", quantity)]
ginirealtshr_plot_data[, quantity := gsub("disposable", "Disposable", quantity)]
ginirealtshr_plot_data[, quantity := gsub("final", "Final", quantity)]

ginirealtshr_plot_data[, quantity :=
                           factor(quantity,
                                  levels = c("Gini coefficients \u2013 Disposable income",
                                             "Gini coefficients \u2013 Final income",
                                             "Redistributive effects \u2013 Market to Disposable",
                                             "Redistributive effects \u2013 Disposable to Final"))]

ginirealtshr_plot_data[, sharing_assumption :=
                           factor(sharing_assumption,
                                  levels = c("Intra-family mOECD",
                                             "Intra-family equal",
                                             "Intra-household mOECD"))]

ginirealtshr_plot_data[, age := factor(age, levels = unique(age))]
ginirealtshr_age_labels <- ginirealtshr_plot_data[, levels(age)]

setorder(ginirealtshr_plot_data, quantity, sharing_assumption, age)

ginirealtshr_plot_data[, x := as.numeric(age)]
ginirealtshr_plot_data[sharing_assumption == "Intra-family mOECD", x := x - 0.1]
ginirealtshr_plot_data[sharing_assumption == "Intra-family equal", x := x + 0.0]
ginirealtshr_plot_data[sharing_assumption == "Intra-household mOECD", x := x + 0.1]

ginirealtshr_plot_labels_panel <-
  ginirealtshr_plot_data[age == "0-4" & sharing_assumption == "Intra-family mOECD"]
ginirealtshr_plot_labels_panel[, gini_coefficient := c(60, 60, 60, 15)]
ginirealtshr_plot_labels_panel[, label := paste0("(", letters[1:4], ")")]

p <-
  ggplot(ginirealtshr_plot_data,
         aes(x = x, y = gini_coefficient,
             colour = sharing_assumption, shape = sharing_assumption)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = gini_coefficient - gini_coefficient_ase,
                    ymax = gini_coefficient + gini_coefficient_ase),
                width = 0,
                show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(ginirealtshr_age_labels), labels = ginirealtshr_age_labels) +
  labs(x = "Age group",
       y = "Redistributive effect                Gini coefficient\u2003") +
  scale_colour_manual(values = fi_palette) +
  facet_wrap(~ quantity, nrow = 2L, scales = "free_y") +
  geom_text(data = ginirealtshr_plot_labels_panel, nudge_x = 0.0, nudge_y = 0,
            label = ginirealtshr_plot_labels_panel$label, colour = "black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 21, hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white", linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facetted_pos_scales(y = list(quantity == "Gini coefficients \u2013 Disposable income" ~
                                 scale_y_continuous(limits = c(15, 60), breaks = seq(20, 60, 20)),
                               quantity == "Gini coefficients \u2013 Final income" ~
                                 scale_y_continuous(limits = c(15, 60), breaks = seq(20, 60, 20)),
                               quantity == "Redistributive effects \u2013 Market to Disposable" ~
                                 scale_y_continuous(limits = c(0, 60), breaks = seq(0, 60, 20)),
                               quantity == "Redistributive effects \u2013 Disposable to Final" ~
                                 scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 5))))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
