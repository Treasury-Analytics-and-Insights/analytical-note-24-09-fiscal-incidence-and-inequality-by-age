library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure03_IncomeSupportDirectTaxes.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic %in% c("Average Income support", "Average Direct taxes") &
                  `Sharing assumption` == "No sharing"),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`)]

isdtax_plot_data <-
  data_subset[, .(income_or_tax_type = gsub(".*\\s(\\w+\\s\\w+)", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  amount = Estimate,
                  amount_ase = `Sampling error`)]

isdtax_plot_data[, income_or_tax_type := factor(income_or_tax_type,
                                                levels = c("Income support", "Direct taxes"))]

isdtax_plot_data[, age := factor(age, levels = unique(age))]
isdtax_age_labels <- isdtax_plot_data[, levels(age)]

setorder(isdtax_plot_data, income_or_tax_type, age)

isdtax_plot_labels_panel <-
  isdtax_plot_data[age == "0-4", .(income_or_tax_type, sharing_assumption, amount, age)]

isdtax_plot_labels_panel[, label := paste0("(", letters[1:2], ")")]
isdtax_plot_labels_panel[, amount := 0.98 * c(2.4e4, 2.4e4)]

p <-
  ggplot(isdtax_plot_data,
         aes(x = as.integer(age), y = amount,
             colour = sharing_assumption, shape = sharing_assumption)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = amount - amount_ase, ymax = amount + amount_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(isdtax_age_labels), labels = isdtax_age_labels) +
  scale_y_continuous(labels = scales::dollar_format(), limits = c(0, 2.4e4),
                     breaks = seq(0, 2.4e4, 6e3)) +
  labs(x = "Age group",
       y = "Average amount ($2019)") +
  scale_colour_manual(values = fi_palette) +
  geom_text(data = isdtax_plot_labels_panel, nudge_x = -0.5, nudge_y = 0,
            label = isdtax_plot_labels_panel$label, colour = "black", size = 5) +
  facet_wrap(~ income_or_tax_type, nrow = 1L, scales = "free_y") +
  theme_bw() +
  theme(legend.position = "none",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_text(size = 20, hjust = 0),
        strip.background = element_rect(colour = "white", fill = "white", linetype = "solid"),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 13, height = 4, units = "in", dpi = 600, type = "cairo")
