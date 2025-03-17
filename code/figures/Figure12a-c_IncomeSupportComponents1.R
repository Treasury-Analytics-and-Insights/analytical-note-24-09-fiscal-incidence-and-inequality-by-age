library(data.table)
library(ggplot2)
library(ggh4x)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure12a-c_IncomeSupportComponents1.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[Statistic %in% c("Average Working-age support",
                                "Average Housing support",
                                "Average Working for Families") &
                 `Sharing assumption` == "No sharing",
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`,
                 Note)]

iscpts1_plot_data <-
  data_subset[, .(income_or_tax_type = gsub("\\w+\\s(.*)", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`,
                  note = Note)]

iscpts1_plot_data[, income_or_tax_type :=
                      factor(income_or_tax_type,
                             levels = c("Working-age support",
                                        "Housing support",
                                        "Working for Families"))]

iscpts1_plot_data[, age := factor(age, levels = unique(age))]
iscpts1_age_labels <- iscpts1_plot_data[, levels(age)]

setorder(iscpts1_plot_data, income_or_tax_type, age)

iscpts1_plot_labels <-
  iscpts1_plot_data[, .(income_or_tax_type, amount, sharing_assumption, age, note)]
iscpts1_plot_labels[note == "Suppressed", label := "S"]
iscpts1_plot_labels[, `:=`(amount = 0, note = NULL)]

iscpts1_plot_labels_was <- iscpts1_plot_labels[income_or_tax_type == "Working-age support"]

iscpts1_plot_labels_hou <- iscpts1_plot_labels[income_or_tax_type == "Housing support"]

iscpts1_plot_labels_wff <- iscpts1_plot_labels[income_or_tax_type == "Working for Families"]

iscpts1_plot_labels_panel <-
  iscpts1_plot_data[age == "0-4", .(income_or_tax_type, sharing_assumption, amount, age)]

iscpts1_plot_labels_panel[, label := paste0("(", letters[1:3], ")")]
iscpts1_plot_labels_panel[, amount := 0.98 * c(3e3, 1.5e3, 2.2e3)]

p <-
  ggplot(iscpts1_plot_data,
         aes(x = as.integer(age), y = amount,
             colour = sharing_assumption, shape = sharing_assumption)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = amount - amount_ase, ymax = amount + amount_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(1L, length(iscpts1_age_labels), by = 2L),
                     labels = iscpts1_age_labels[seq(1L, length(iscpts1_age_labels), by = 2L)]) +
  scale_y_continuous(labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average income support ($2019)") +
  scale_colour_manual(values = fi_palette) +
  geom_text(data = iscpts1_plot_labels_was, nudge_x = 0, nudge_y = 0,
            label = iscpts1_plot_labels_was$label, colour = fi_palette[1], size = 5) +
  geom_text(data = iscpts1_plot_labels_hou, nudge_x = 0, nudge_y = 0,
            label = iscpts1_plot_labels_hou$label, colour = fi_palette[1], size = 5) +
  geom_text(data = iscpts1_plot_labels_wff, nudge_x = 0, nudge_y = 0,
            label = iscpts1_plot_labels_wff$label, colour = fi_palette[1], size = 5) +
  geom_text(data = iscpts1_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = iscpts1_plot_labels_panel$label, colour = "black", size = 5) +
  facet_wrap(~ income_or_tax_type, nrow = 1L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 16),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 20, hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white", linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1, size = 11)) +
  facetted_pos_scales(y = list(income_or_tax_type == "Working-age support" ~
                                 scale_y_continuous(limits = c(0, 3e3),
                                                    breaks = seq(0, 3e3, 1e3),
                                                    labels = scales::dollar_format()),
                               income_or_tax_type == "Housing support" ~
                                 scale_y_continuous(limits = c(0, 1.5e3),
                                                    breaks = seq(0, 1.5e3, 5e2),
                                                    labels = scales::dollar_format()),
                               income_or_tax_type == "Working for Families" ~
                                 scale_y_continuous(limits = c(0, 2.2e3),
                                                    breaks = seq(0, 2e3, 5e2),
                                                    labels = scales::dollar_format())))

ggsave(p, filename = figure_filename,
       width = 13, height = 4, units = "in", dpi = 600, type = "cairo")
