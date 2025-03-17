library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure11b_BenefitReceipt.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Jobseeker Support recipients",
                                "Supported Living Payment recipients",
                                "Sole Parent Support recipients",
                                "Non-qualified partner NZS recipients"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`, Note)]

benr_plot_data <-
  data_subset[, .(benefit = gsub("(.*)\\s(.*)$", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  benr = Estimate,
                  benr_ase = `Sampling error`,
                  note = Note)]

benr_plot_data[benefit == "Jobseeker Support", benefit := "JSS"]
benr_plot_data[benefit == "Supported Living Payment", benefit := "SLP"]
benr_plot_data[benefit == "Sole Parent Support", benefit := "SPS"]

benr_plot_data[benefit == "Non-qualified partner NZS", benefit := "Non-qualified\npartner NZS"]

benr_plot_data[, benefit := factor(benefit,
                                   levels = c("JSS", "SLP", "SPS", "Non-qualified\npartner NZS"))]

benr_plot_data[, age := factor(age, levels = unique(age))]
benr_age_labels <- benr_plot_data[, levels(age)]

setorder(benr_plot_data, benefit, age)

benr_plot_data[, x := as.numeric(age)]

benr_plot_labels <- benr_plot_data[, .(benr, benefit, x, note)]
benr_plot_labels[note == "Suppressed", label := "S"]
benr_plot_labels[, `:=`(benr = 0, note = NULL)]

benr_plot_labels_jss <- benr_plot_labels[benefit == "JSS"]
benr_plot_labels_slp <- benr_plot_labels[benefit == "SLP"]
benr_plot_labels_sps <- benr_plot_labels[benefit == "SPS"]
benr_plot_labels_nqp <- benr_plot_labels[benefit == "Non-qualified\npartner NZS"]

benr_plot_labels_slp[label == "S",  benr := c(0.75, 0, 0, 0, 0, 0.75, 0.75, 0.75)]
benr_plot_labels_sps[label == "S",  benr := c(1.5, 0, 0, 0, 0, 0.75, 1.5, 1.5, 1.5)]
benr_plot_labels_nqp[label == "S",  benr := c(2.25, 0.75, 0.75, 0, 0, 0.75, 0.75, 0.75, 0.75)]

benr_plot_labels_panel <- benr_plot_data[age == "15-19" & benefit == "JSS"]

benr_plot_labels_panel[, label := "(b)"]
benr_plot_labels_panel[, benr := 0.98 * 15]

p <-
  ggplot(benr_plot_data, aes(x = x, y = benr, colour = benefit, shape = benefit)) +
  geom_point(size = 3) +
  geom_line(show.legend = FALSE) +
  geom_errorbar(aes(ymin = benr - benr_ase, ymax = benr + benr_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(benr_age_labels), labels = benr_age_labels) +
  scale_y_continuous(limits = c(0, 15), breaks = seq(0, 15, 3)) +
  labs(x = "Age group",
       y = "Payment recipients (%)") +
  scale_colour_manual(values = fi_palette) +
  scale_shape_manual(values = c(16L, 17L, 15L, 18L)) +
  geom_text(data = benr_plot_labels_jss, nudge_x = 0, nudge_y = 0,
            label = benr_plot_labels_jss$label, colour = fi_palette[1], size = 4) +
  geom_text(data = benr_plot_labels_slp, nudge_x = 0, nudge_y = 0,
            label = benr_plot_labels_slp$label, colour = fi_palette[2], size = 4) +
  geom_text(data = benr_plot_labels_sps, nudge_x = 0, nudge_y = 0,
            label = benr_plot_labels_sps$label, colour = fi_palette[3], size = 4) +
  geom_text(data = benr_plot_labels_nqp, nudge_x = 0, nudge_y = 0,
            label = benr_plot_labels_nqp$label, colour = fi_palette[4], size = 4) +
  geom_text(data = benr_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = benr_plot_labels_panel$label, colour = "black", size = 5) +
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
