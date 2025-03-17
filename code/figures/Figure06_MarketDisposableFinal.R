library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure06_MarketDisposableFinal.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic == "Average Market income" &
                  `Sharing assumption` == "No sharing") |
               (Statistic %in% c("Average Disposable income", "Average Final income") &
                  `Sharing assumption` == "Intra-family mOECD"),
               .(`Group or category`, Statistic, Estimate, `Sampling error`)]

mdf_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s(\\w+\\s\\w+)", "\\1", Statistic),
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  income = Estimate,
                  income_ase = `Sampling error`)]

mdf_plot_data[, income_type := factor(income_type,
                                      levels = c("Market income",
                                                 "Disposable income",
                                                 "Final income"))]

mdf_plot_data[, age := factor(age, levels = unique(age))]
mdf_age_labels <- mdf_plot_data[, levels(age)]

setorder(mdf_plot_data, income_type, age)

p <-
  ggplot(mdf_plot_data,
         aes(x = as.integer(age), y = income,
             colour = income_type, shape = income_type)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = income - income_ase, ymax = income + income_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(mdf_age_labels), labels = mdf_age_labels) +
  scale_y_continuous(limits = c(0, 8.5e4), breaks = seq(0, 8e4, 2e4),
                     labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average income ($2019)") +
  scale_colour_manual(values = fi_palette) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
