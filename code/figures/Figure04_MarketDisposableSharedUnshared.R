library(data.table)
library(ggplot2)

fi_palette <- c("#4472C4", "#ED7D31", "#A5A5A5", "#FFC000", "#5B9BD5")

figures_data_filename <- "../../data/data.csv"
figure_filename <- "../../figures/Figure04_MarketDisposableSharedUnshared.png"

figures_data <- fread(figures_data_filename)

data_subset <-
  figures_data[(Statistic == "Average Market income" & `Sharing assumption` == "No sharing") |
               (Statistic == "Average Disposable income" &
                  `Sharing assumption` %in% c("No sharing", "Intra-family mOECD")),
               .(`Group or category`, Statistic, `Sharing assumption`, Estimate, `Sampling error`)]

mdshrdunshrd_plot_data <-
  data_subset[, .(income_type = gsub(".*\\s(\\w+\\s\\w+)", "\\1", Statistic),
                  sharing_assumption = `Sharing assumption`,
                  age = gsub(".*\\s(.*)", "\\1", `Group or category`),
                  income = Estimate,
                  income_ase = `Sampling error`)]

mdshrdunshrd_plot_data[income_type == "Market income", income_and_sharing_case := "Market"]
mdshrdunshrd_plot_data[income_type == "Disposable income" &
                         sharing_assumption == "No sharing",
                       income_and_sharing_case := "Disposable - No sharing"]
mdshrdunshrd_plot_data[income_type == "Disposable income" &
                         sharing_assumption == "Intra-family mOECD",
                       income_and_sharing_case := "Disposable - Intra-family mOECD sharing"]

mdshrdunshrd_plot_data[, income_and_sharing_case :=
                           factor(income_and_sharing_case,
                                  levels = c("Market",
                                             "Disposable - No sharing",
                                             "Disposable - Intra-family mOECD sharing"))]

mdshrdunshrd_plot_data[, age := factor(age, levels = unique(age))]
mdshrdunshrd_age_labels <- mdshrdunshrd_plot_data[, levels(age)]

setorder(mdshrdunshrd_plot_data, income_and_sharing_case, age)

  mdshrdunshrd_plot_legend_labels <-
    c("Market income",
      sprintf("Disposable income\n\u2014 No sharing"),
      sprintf("Disposable income\n\u2014 Intra-family sharing"))
  
p <-
  ggplot(mdshrdunshrd_plot_data,
         aes(x = as.integer(age), y = income,
             colour = income_and_sharing_case, shape = income_and_sharing_case)) +
  geom_point(size = 3) +
  geom_line(linewidth = 1, show.legend = FALSE) +
  geom_errorbar(aes(ymin = income - income_ase, ymax = income + income_ase),
                width = 0.3, show.legend = FALSE) +
  scale_x_continuous(breaks = 1:length(mdshrdunshrd_age_labels),
                     labels = mdshrdunshrd_age_labels) +
  scale_y_continuous(limits = c(0, 8.5e4), breaks = seq(0, 8e4, 2e4),
                     labels = scales::dollar_format()) +
  labs(x = "Age group",
       y = "Average income ($2019)") +
  scale_colour_manual(values = fi_palette, labels = mdshrdunshrd_plot_legend_labels) +
  scale_shape_discrete(labels = mdshrdunshrd_plot_legend_labels) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        text = element_text(size = 20),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(p, filename = figure_filename,
       width = 13, height = 8, units = "in", dpi = 600, type = "cairo")
