library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure11a_LabourForceParticipationRate.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Labour force participation rate - Sole adult in family",
                                "Labour force participation rate - Principal earner in couple",
                                "Labour force participation rate - Partner of principal earner",
                                "Labour force participation rate - Dependent"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`, Note)]

lfpr_plot_data <-
  data_subset[, .(role = gsub("(.*)\\s-\\s(.*)", "\\2", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  lfpr = Estimate,
                  lfpr_ase = `Sampling error`,
                  note = Note)]

lfpr_plot_data[role == "Sole adult in family", role := "Sole adult"]
lfpr_plot_data[role == "Principal earner in couple", role := "Couple\n\u2014 Principal"]
lfpr_plot_data[role == "Partner of principal earner", role := "Couple\n\u2014 Partner"]

lfpr_plot_data[, role := factor(role,
                                levels = c("Sole adult",
                                           "Couple\n— Principal",
                                           "Couple\n— Partner",
                                           "Dependent"))]

lfpr_plot_data[, age := factor(age, levels = unique(age))]
lfpr_age_labels <- lfpr_plot_data[, levels(age)]

setorder(lfpr_plot_data, role, age)

lfpr_plot_data[, x := as.numeric(age)]

lfpr_plot_labels <- lfpr_plot_data[, .(lfpr, role, x, note)]
lfpr_plot_labels[note == "Suppressed", label := "S"]
lfpr_plot_labels[, `:=`(lfpr = 0, note = NULL)]

lfpr_plot_labels_soa <- lfpr_plot_labels[role == "Sole adult"]
lfpr_plot_labels_cpr <- lfpr_plot_labels[role == "Couple\n\u2014 Principal"]
lfpr_plot_labels_cpa <- lfpr_plot_labels[role == "Couple\n\u2014 Partner"]

lfpr_plot_labels_cpr[label == "S",  lfpr := c(0, 5)]
lfpr_plot_labels_cpa[label == "S",  lfpr := c(5, 5, 10)]

lfpr_plot_labels_sup <- lfpr_plot_labels[1]
lfpr_plot_labels_sup[, label := "S - Suppressed"]
lfpr_plot_labels_sup[, lfpr := 95]

lfpr_plot_labels_panel <- lfpr_plot_data[age == "15-19" & role == "Sole adult"]

lfpr_plot_labels_panel[, label := "(a)"]
lfpr_plot_labels_panel[, lfpr := 0.98 * 102]

p <-
  ggplot(lfpr_plot_data, aes(x = x, y = lfpr, colour = role, shape = role)) +
  geom_point(size = 3) +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin = lfpr - lfpr_ase, ymax = lfpr + lfpr_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(lfpr_age_labels), labels = lfpr_age_labels) +
  scale_y_continuous(limits = c(0, 102), breaks = seq(0, 100, 25)) +
  labs(x = "Age group",
       y = "Labour force partic. rate (%)") +
  scale_colour_manual(values = fi_palette) +
  scale_shape_manual(values = c(16L, 17L, 15L, 18L)) +
  geom_text(data = lfpr_plot_labels_soa, nudge_x = 0, nudge_y = 0,
            label = lfpr_plot_labels_soa$label, colour = fi_palette[1], size = 4) +
  geom_text(data = lfpr_plot_labels_cpr, nudge_x = 0, nudge_y = 0,
            label = lfpr_plot_labels_cpr$label, colour = fi_palette[2], size = 4) +
  geom_text(data = lfpr_plot_labels_cpa, nudge_x = 0, nudge_y = 0,
            label = lfpr_plot_labels_cpa$label, colour = fi_palette[3], size = 4) +
  geom_text(data = lfpr_plot_labels_sup, nudge_x = 11.5, nudge_y = 0,
            label = lfpr_plot_labels_sup$label, colour = "black", size = 5) +
  geom_text(data = lfpr_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = lfpr_plot_labels_panel$label, colour = "black", size = 5) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 15),
        plot.title = element_text(size = 16),
        legend.text = element_text(size = 11),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 6, height = 4, units = "in", dpi = 600, type = "cairo")
